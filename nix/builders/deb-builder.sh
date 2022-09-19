source $stdenv/setup
PATH=$dpkg/bin:$PATH

dpkg -x $src unpacked

cp -r unpacked $out/

cp -r $out/usr/local/bin $out/