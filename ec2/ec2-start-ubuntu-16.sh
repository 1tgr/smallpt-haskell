#!/bin/sh

function launchInstance {
  hostname=$1

  until ssh -o StrictHostKeyChecking=no root@$hostname "echo SSH is ready"; do
    echo "Establish ssh: retrying"
    sleep 1
  done

  scp ~/.awssecret ~/.twidgerc root@$hostname:/root/
  ssh root@$hostname <<EOF
cd /tmp
apt-get update
apt-get -y install libexpat1-dev libfontconfig1-dev libfreetype6-dev libgmp3-dev libjpeg-dev libpng-dev twidge zlib1g-dev
EOF

  ssh root@$hostname <<EOF
cd /tmp
curl http://timkay.com/aws/aws -o aws
perl aws --install

s3get partario/gd-2.0.35.tar.gz | tar -zx
(
  cd gd-2.0.35
  ./configure
  make
  make install
  ldconfig
)

s3get partario/ghc-6.10.4-i386-unknown-linux-n.tar.bz2 | bunzip2 | tar -x
(
  cd ghc-6.10.4
  ./configure
  make install
)

s3get partario/cabal-install-0.6.2.tar.gz | tar -xz
(
  cd cabal-install-0.6.2
  ./bootstrap.sh
)

ln /root/.cabal/bin/cabal /usr/local/bin
cabal update
cp /root/.cabal/config /root/.cabal/config.old
sed -e "s,-- symlink-bindir:,symlink-bindir: /usr/local/bin," /root/.cabal/config.old > /root/.cabal/config
twidge update "@tim_g_robinson $hostname is ready"
EOF
}

aws describe-instances --simple | grep -qv terminated || aws run-instances ami-ccf615a5 -g Admin -k Tim -n 16 --simple

echo "Waiting for all instances to start"
while aws describe-instances --simple | grep -q pending; do
  sleep 1
done

log=/tmp/ec2-start-ubuntu-16.log
[ -e $log  ] && rm $log

for hostname in `aws describe-instances --simple | grep running | cut -f 3`; do
  ssh-keygen -R $hostname
  launchInstance $hostname 2>&1 >> $log &  
done

echo "Waiting for all instances to finish installing"
wait
