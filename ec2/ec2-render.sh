#!/bin/sh
twidge update "`date +%T` Render is starting"
smallpt=dist/build/smallpt/smallpt
workers="\"$smallpt --runWorker\""
if $smallpt -samples 8000 $workers; then
  s3put --public partario/public/image.png image.png
  twidge update "`date +%T` Render is finished - http://s3.amazonaws.com/partario/public/image.png"
else
  twidge update "`date +%T` Render failed"
fi