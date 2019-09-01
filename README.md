# rmExif

Remove exif form a jpeg file.

# Usage

Put an input jpeg file path and an output jpec file path.

example :
```
rmExif-exe -f ./IMG_wITH_EXIF.jpg ./IMG_WITHOUT_EXIF.jpg
```

or if you put an output, an output file neme is an input filne name plus '~'.

example :
```
rmExif-exe -f ./IMG_wITH_EXIF.jpg
```

You get a file named "./IMG_WITH_EXIF~.jpg"

or put "-d" and a directory path, and then you can process all jpeg files in the directory.

example :
```
rmExif-exe -d "./IMG_FILES"
```
# Installation

Get an exe file form a bin directory or clone this repo.