#!/bin/sh
smallpt=dist/build/smallpt/smallpt

function makeWorkers {
  yes "\"$smallpt -r\"" | head -n 8
  aws describe-instances --simple | awk '$2 == "running" { printf "\"ssh root@%s smallpt -r\"\n", $3 }'
}

twidge update "`date +%T` Render is starting"
if makeWorkers | xargs $smallpt --samples 8000
then
  s3put --public partario/public/image.png image.png
  twidge update "`date +%T` Render is finished - http://s3.amazonaws.com/partario/public/image.png"
else
  twidge update "`date +%T` Render failed"
fi