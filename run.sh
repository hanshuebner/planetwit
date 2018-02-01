#!/bin/sh

while true
do
    sbcl < /dev/null \
        --disable-debugger --load load.lisp --eval '(planetwit:main)' --eval '(sb-ext:quit)' \
        | tee /dev/tty \
        | mail -s "planetwit crashed" hans.huebner@gmail.com
    sleep 30
done