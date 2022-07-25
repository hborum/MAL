set steps=100   1>out/bench.txt
set runs=3      1>>out/bench.txt
set iterprrun=1 1>>out/bench.txt
set warmups=1   1>>out/bench.txt
set groups=10   1>>out/bench.txt

call build --TEST --release
call run-release.bat --compile examples\statehandler.fmal 1>>out/bench.txt
call run-release.bat --test  1>>out/bench.txt
call build --release
call run-release.bat --benchmark %steps% %runs% %iterprrun% %warmups% %groups%   50 1>>out/bench.txt
call run-release.bat --benchmark %steps% %runs% %iterprrun% %warmups% %groups%  500 1>>out/bench.txt
