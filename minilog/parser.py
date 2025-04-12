#!/usr/bin/env python
import json

# thread_id: 1
# ---*---------*-----
# thread_id: 2
# --------*------*---
with open("out.ndjson", "r") as f:
    for line in f:
        json_data = json.loads(line)
        print(json_data["process"])
