ROADMAP
=======

This document explains in as much details as possible the
list of planned changes and work to be done on the Cowboy
server. It is intended to be exhaustive but some elements
might still be missing.

1.0
---

 *  Parse support for all standard HTTP/1.1 headers

 *  Support for multipart requests and responses

 *  Add Range support to REST

 *  Complete the user guide

1.1
---

 *  Check if using maps instead of a record improves performance

2.0
---

 *  Support for HTTP/2.0

 *  Simplify cowboy_req access functions

    They do not need to return Req. So let's not.

Under consideration
-------------------

 *  Convenience API for extracting query string and body
    information, similar to PHP's $_GET, $_POST and $_FILES
