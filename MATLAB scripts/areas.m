% Plot distance against area

% Open the area function files and read into arrays
fid = fopen('VT04 Hard Oral.txt','r');
if( fid == -1)
    disp('Error opening the file');
else
    area1 = fscanf(fid, '%f');
    fclose(fid);
end

fid=fopen('VT04 Hard Pharyngeal.txt','r');
if( fid == -1)
    disp('Error opening the file');
else
    area2 = fscanf(fid, '%f');fclose(fid);
end

% Create vector of area1 and area2 note that node 15 is an average
% of measurements from last oral and first pharyngeal calculations
NumberofNodes = 15;
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
%node as it will always be zero (glottis).
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