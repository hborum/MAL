del /Q nugets\*
del /Q nugets.zip
cd MAL
nuget pack
cd ..\Stubs
nuget pack
cd ..\Gen
nuget pack
cd ..
move MAL\MAL.1.0.0.nupkg nugets\MAL.1.0.0.nupkg 
move Stubs\Stub.0.0.0.nupkg nugets\Stub.0.0.0.nupkg
move Gen\Gen.1.0.0.nupkg nugets\Gen.1.0.0.nupkg	
7z a -tzip nugets.zip nugets\*