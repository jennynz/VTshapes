function [lengthtot] =totallengthcalc(oraloutputFile,pharyngealoutputFile,outputFile)
%Number of Nodes to process
numberofNodes = 15;
fid=fopen(oraloutputFile,'r');
if( fid == -1)
    disp('Error opening the file 1 ');
    
else
    olength=fscanf(fid,'%e',inf);
end


fid=fopen(pharyngealoutputFile,'r');
if( fid == -1)
    disp('Error opening the file 1 ');
    
else
    plength=fscanf(fid,'%e',inf);
end

lengthtot(1) = 0;

%Combining oral and pharyngeal length vectors (note that we only have 29
%since the end of the oral cavity is effectively the same place as the
%pharyngeal cavity

for i=1:2*numberofNodes-1
if i<=numberofNodes
    lengthtot(i)=olength(i);
elseif i>=numberofNodes+1
    lengthtot(i)=olength(numberofNodes)+plength(i-(numberofNodes-1));
end
end

%Write the length vector to a file "length.txt"
fid = fopen(outputFile,'w');
if( fid == -1)
    disp('Error opening the output  file 1b');
else
    fprintf(fid,'%d\t\n',lengthtot);
    st = fclose(fid);
end