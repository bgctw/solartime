## Note
According to an email from Kurt Hornik, examples/vignettes changed to use timezone 
"Europe/Berlin" rather than "CET", 
which is not supported any more on specific debian machines.

Additionally, I included an Authors@R comment and updated the Authors field to
be compatible with that.

Additionally, the package now gives a warning to the user, when provided with
a non-scalar geo-location when computing the sun-positions and works with the first
entry of the provided locations. This addresses a failing vignette of the
downstream move package.





  