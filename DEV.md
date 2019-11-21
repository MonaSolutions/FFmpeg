To compile and install libwebgroup.so :
make
sudo make install

To compile ffmpeg :
./configure --disable-x86asm --enable-libspeex --enable-libx264 --enable-libx265 --enable-libsrt --enable-libwebgroup --enable-debug --enable-gpl
make

To test webgroup :
./ffmpeg -re -i ./Sintel.ts -c copy -f flv wg://0.0.0.0:1234/test

