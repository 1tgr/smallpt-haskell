#!/bin/sh
cabal configure
cabal build
cabal sdist
s3put partario/smallpt-1.0.tar.gz dist/smallpt-1.0.tar.gz

function deployInstance {
  hostname=$1
  ssh root@$hostname <<EOF
s3get partario/smallpt-1.0.tar.gz | tar -zx
(cd smallpt-1.0 && /root/.cabal/bin/cabal install)
twidge update "@tim_g_robinson $hostname installed"
EOF
}

log=/tmp/ec2-deploy.log
[ -e $log ] && rm $log

for hostname in `aws describe-instances --simple | grep running | cut -f 3`; do
  deployInstance $hostname 2>&1 >> $log &
done

echo "Waiting for all instances to deploy"
wait