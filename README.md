About Shirakumo-Dist
--------------------
This is an application to help manage the Shirakumo [Quicklisp](http://www.quicklisp.org/) dist. It contains a number of tools for handling git repositories and a small wrapper around [Quickdist](https://github.com/orivej/quickdist) to run a dist. Some special changes apply for Shirakumo's libraries. You can probably extract most of the tools in here for your own purposes to create a quick and easy dist for your own systems.

Installing
----------
Adding a dist to quicklisp is very easy. 

    (ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")

And that's it. All software from the dist should now be available for quickloading.

The Shirakumo dist hosted by this primarily contains Radiance and its affiliated projects. They cannot be distributed via the standard quicklisp dist, due to the special loading behaviour that breaks quicklisp requirements. The Shirakumo dist may occasionally also deliver hot-patches for select libraries. It is however not updated on any regular schedule, and does not make any efforts to test compatibility with the rest of the ecosystem at the time of release.
