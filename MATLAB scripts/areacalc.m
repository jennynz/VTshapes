function [area,area1,area2]=areacalc(oralinputArea,pharyngealinputArea)

NumberofNodes=15;
%Open the file for reading
fid=fopen(oralinputArea,'r');
if( fid == -1)
    disp('Error opening the file');
else
    area1 = fscanf(fid, '%f');
    st = fclose(fid);
% close the file once we are finished with it
end

%Open the file for reading
fid=fopen(pharyngealinputArea,'r');
if( fid == -1)
    disp('Error opening the file');
else
    area2 = fscanf(fid, '%f');
    st = fclose(fid);
% close the file once we are finished with it
end

%Create vector of area1 and area2 note that node 15 is an average
%of measurements from oral and pharyngeal calculations
area(1)=0;
for i=1:2*NumberofNodes-1
    if i<NumberofNodes
        area(i)=area1(i);
    elseif i>NumberofNodes
        area(i)=area2(i-(NumberofNodes-1));
    else area(i)=(area1(NumberofNodes)+area2(1))/2;
    end
end
%taking into account inability to measure some slices by using the
%following node as an approximation. Note that we do not go to the final
%node as it will always be zero.
for j=1:2*NumberofNodes-3
if area(j)<1
    area(j)=area(j+1);
end
if area(j+1)<1
    area(j+1)=area(j+2); 
    area(j+1)=area(j);
end
end
%final area node must be zero for closed tube approximation
area(2*NumberofNodes-1)=0;

%oral and pharyngeal areas for volume calculation needs to take into
%account sharing of middle node. This will not affect plot since it does
%not write to 'area'
for k=1:2*NumberofNodes-1
    if k<NumberofNodes
        area1(k)=area(k);
    elseif i>NumberofNodes
        area2(k-(NumberofNodes-1))=area(k);
    end
end
area1(NumberofNodes)=area(NumberofNodes);
area2(1)=area1(NumberofNodes);

