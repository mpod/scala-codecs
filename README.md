# scala-codecs

Here I explore for self-educational purposes popular binary file formats. Only GZIP decoder has been implemented so far.

## GZIP decoder

Following documents specify GZIP file format:

* [DEFLATE compressed data format specification](https://www.ietf.org/rfc/rfc1951.txt)
* [GZIP file format specification](https://www.ietf.org/rfc/rfc1952.txt)

An example .gz file has been manually crafted and placed in examples directory (abc.gz). The file demonstrates use of 
all three types of blocks in DEFLATE data format: non-compressed, compressed with fixed Huffman codes, and compressed 
with dynamic Huffman codes. Boundaries between those blocks can be found with the help of `DeflateDecoder.log` method. 

### Running ###

    $ sbt "run examples/abc.gz"
