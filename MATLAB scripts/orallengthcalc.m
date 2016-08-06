function [olength] =orallengthcalc(inputFile,outputFile)
%Number of Nodes to process
numberOfNodes = 15;

%Open the file for reading
fid=fopen(inputFile,'r');
if( fid == -1)
    disp('Error opening the file 1 ');
    
else
    i = 1;
    %Discared the header of the node file
    line = fgetl(fid);
    while ((line ~= -1) & (i < 7))
        line = fgetl(fid);
        i = i + 1;
    end
    expectInt = 1;
    j = 1;
    
    %Save the node position values in to a Matrix
    while ((line ~= -1) & (i >= 7))
        %Discard the line which states the relavent node which the position
        %values apply to.
        if expectInt <= 3
           line = fgetl(fid);
           value(j,expectInt) = sscanf(line, '%e', 1);
           expectInt = expectInt + 1;
        else
            line = fgetl(fid);
            expectInt = 1;
            j = j + 1;
        end
    end
    
    st = fclose(fid);
% close the file once we are finished with it
end

%Extract X values and Y values from the value matrix
xValues = value(:,1);
yValues = value(:,2);

%Set up length vector as the first node is at a relavent position of 0
olength(1) = 0;
i = 1;
z = numberOfNodes;

%Work out the distance from the next node to the previous node and add to
%that the distance from the original node (reference node)
while (i < numberOfNodes)
    
    xdifference = xValues(z,1) - xValues(z - 1,1);
    ydifference = yValues(z,1) - yValues(z - 1,1);
    olength(i + 1) = olength(i) + sqrt(xdifference^2 + ydifference^2);
    i = i + 1;
    z = z - 1;
end

%Write the length vector to a file "length.txt"
fid = fopen(outputFile,'w');
if( fid == -1)
    disp('Error opening the output file');
   
else
    fprintf(fid,'%d\t\n',olength);
    st = fclose(fid);
end
