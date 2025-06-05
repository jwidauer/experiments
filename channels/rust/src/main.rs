use std::{
    fmt::Debug,
    sync::{
        self,
        atomic::{AtomicUsize, Ordering},
    },
};

#[derive(Debug)]
struct Queue<T: Default + Copy, const N: usize> {
    buf: [T; N],
    read_idx: AtomicUsize,
    write_idx: AtomicUsize,
}

impl<T: Default + Copy, const N: usize> Queue<T, N> {
    fn new() -> Queue<T, N> {
        const { assert!(N >= 1, "Queue needs to have at least one element") }
        let arr = core::array::from_fn(|_| T::default());
        Self {
            buf: arr,
            read_idx: AtomicUsize::new(0),
            write_idx: AtomicUsize::new(0),
        }
    }

    fn try_push(&mut self, val: T) -> Result<(), T> {
        if self.is_full() {
            return Err(val);
        }

        let cur_write_idx = self.write_idx.fetch_add(1, Ordering::SeqCst) % N;
        self.buf[cur_write_idx] = val;

        Ok(())
    }

    fn try_pop(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }

        let read_idx = self.read_idx.fetch_add(1, Ordering::SeqCst) % N;
        Some(self.buf[read_idx])
    }

    fn len(&self) -> usize {
        self.write_idx.load(Ordering::Relaxed) - self.read_idx.load(Ordering::Relaxed)
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn is_full(&self) -> bool {
        self.len() == N
    }
}

fn main() {
    let mut queue = Queue::<u8, 3>::new();

    assert_eq!(queue.try_push(1), Ok(()));
    assert_eq!(queue.try_pop(), Some(1));
    assert_eq!(queue.try_push(2), Ok(()));
    assert_eq!(queue.try_pop(), Some(2));
    assert_eq!(queue.try_push(3), Ok(()));
    assert_eq!(queue.try_pop(), Some(3));
    assert_eq!(queue.try_push(4), Ok(()));
    assert_eq!(queue.try_push(5), Ok(()));
    assert_eq!(queue.try_push(6), Ok(()));
    assert_eq!(queue.try_push(7), Err(7));

    let (tx, rx) = sync::mpsc::sync_channel(3);
    tx.send(5).unwrap();
}
