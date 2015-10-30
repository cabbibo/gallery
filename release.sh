GIT_HASH=`git rev-parse --short HEAD`
RELEASE_ZIP=gallery-$GIT_HASH.zip

rm -rf gallery-r/
mkdir gallery-r
stack build
stack install gallery --local-bin-path gallery-r/
cp -R app/ gallery-r/
cp -R dlls/* gallery-r/


7z a -tzip $RELEASE_ZIP gallery-r/
cp $RELEASE_ZIP /c/Users/$USER/Desktop
cp -R gallery-r /c/Users/$USER/Desktop/