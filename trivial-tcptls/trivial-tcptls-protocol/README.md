#trivial-tcptls-protocol

This project will contain the protocol logic that uses a tcp/tls connection

It is a work in progress.

Nothing here to see...  Move along...

This protocol works ok for learning.  I don't really like how I've implemented.

The intent was to explore how the Network.TLS package is used to make a TLS connection.

To run this requires some test certificates.

In this example a Test Certificate Authority was created and the Test Digital Signatures were created with the test authority.

Some things to watch for...
The validation logic is sensitive to leading and trailing spaces in the Domain Distinguided names.

I was testing the client on OSX with the CA certificate stored in the login keychain.

The System.X509.MacOS package only uses the system keychains to validate a certificate.

The trivial-tcptls-protocol package includes a version of getSystemCertificateStore that uses all the available keychains



