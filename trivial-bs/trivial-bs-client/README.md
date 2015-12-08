# trivial-bs-client

This version of the client uses bytestrings instead of handle IO.
It is a naive implementation that doesn't check the number of bytes received and therefore has the happy go luck notion that it has received everything in a single message.
