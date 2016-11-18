# upfind

A program to find files upwards in a directory tree

Returns `0` if a file was found, `1` if no file was found.

## Usage

To find all the files matching `regex` in the closest upwards directory:

`upfind regex`

To find all the files matching `regex` in any upwards directory:

`upfind -k regex`

To find a file matching exactly `filename` in the closest upwards directory:

`upfind -F filename`

To find the closest upwards directory containing a file matching `regex`:

`upfind -d regex`

To find all the upwards directories containing `filename` exactly:

`upfind -d -k -F filename`
