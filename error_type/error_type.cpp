#include <memory>
#include <string>

class Error {
 private:
  class ErrorConcept {
   public:
    virtual ~ErrorConcept() = default;

    virtual std::string to_string() const = 0;
    virtual std::unique_ptr<ErrorConcept> clone() const = 0;
  };

  template <typename ErrorT>
  class ErrorModel : public ErrorConcept {
   public:
    ErrorModel(ErrorT error) : error_(std::move(error)) {}

    virtual std::unique_ptr<ErrorConcept> clone() const
    {
      return std::make_unique<ErrorModel>(*this);
    }

    virtual ~ErrorModel() = default;

    virtual std::string to_string() const { return stringify(error_); }

   private:
    ErrorT error_;
  };

  friend std::string to_string(const Error& error) { return error.pimpl_->to_string(); }

  std::unique_ptr<ErrorConcept> pimpl_;

 public:
  template <typename ErrorT>
  Error(ErrorT error) : pimpl_(std::make_unique<ErrorModel<ErrorT>>(std::move(error)))
  {
  }

  Error(const Error& other) : pimpl_(other.pimpl_->clone()) {}
  Error& operator=(const Error& other)
  {
    other.pimpl_->clone().swap(pimpl_);
    return *this;
  }

  Error(Error&&) = default;
  Error& operator=(Error&&) = default;
};

int main() { return 0; }
