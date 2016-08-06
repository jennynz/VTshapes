%Written by Kalyan Chilukuri
%Adapted by Helen Searle
%Read in the curve.txt nodes and then output the Lengths in to a folder
%called lengths, the lengths are calculated from the node placed near the lips as the zero point
%PLEASE NOTE: If you would like MRI images included in plot, first run
%imageimport.m in this folder.
%Pre-set up
clear;
clc;

%Read in Data in to a Matrix called value

%% File Inputs 
%Length Calculations
inputFile1 = 'Had/Oral/1create_snake/curve.exnode';
inputFile1a = 'Had/Pharyngeal/1create_snake/curve.exnode';
outputFile1='lengths1/hadorallength.txt';
outputFile1a='lengths1/hadpharyngeallength.txt';
outputFile1b='lengths1/hadtotallength.txt';

inputFile2 = 'Hard/Oral/1create_snake/curve.exnode';
inputFile2a = 'Hard/Pharyngeal/1create_snake/curve.exnode';
outputFile2='lengths1/hardorallength.txt';
outputFile2a='lengths1/hardpharyngeallength.txt';
outputFile2b='lengths1/hardtotallength.txt';

inputFile3 = 'Head/Oral/1create_snake/curve.exnode';
inputFile3a = 'Head/Pharyngeal/1create_snake/curve.exnode';
outputFile3='lengths1/headorallength.txt';
outputFile3a='lengths1/headpharyngeallength.txt';
outputFile3b='lengths1/headtotallength.txt';

inputFile4 = 'Heed/Oral/1create_snake/curve.exnode';
inputFile4a = 'Heed/Pharyngeal/1create_snake/curve.exnode';
outputFile4='lengths1/heedorallength.txt';
outputFile4a='lengths1/heedpharyngeallength.txt';
outputFile4b='lengths1/heedtotallength.txt';

inputFile5 = 'Herd/Oral/1create_snake/curve.exnode';
inputFile5a = 'Herd/Pharyngeal/1create_snake/curve.exnode';
outputFile5='lengths1/herdorallength.txt';
outputFile5a='lengths1/herdpharyngeallength.txt';
outputFile5b='lengths1/herdtotallength.txt';

inputFile6 = 'Hid/Oral/1create_snake/curve.exnode';
inputFile6a = 'Hid/Pharyngeal/1create_snake/curve.exnode';
outputFile6='lengths1/hidorallength.txt';
outputFile6a='lengths1/hidpharyngeallength.txt';
outputFile6b='lengths1/hidtotallength.txt';

inputFile7 = 'Hoard/Oral/1create_snake/curve.exnode';
inputFile7a = 'Hoard/Pharyngeal/1create_snake/curve.exnode';
outputFile7='lengths1/hoardorallength.txt';
outputFile7a='lengths1/hoardpharyngeallength.txt';
outputFile7b='lengths1/hoardtotallength.txt';

inputFile8 = 'Hod/Oral/1create_snake/curve.exnode';
inputFile8a = 'Hod/Pharyngeal/1create_snake/curve.exnode';
outputFile8='lengths1/hodorallength.txt';
outputFile8a='lengths1/hodpharyngeallength.txt';
outputFile8b='lengths1/hodtotallength.txt';
inputFile9 = 'Hood/Oral/1create_snake/curve.exnode';

inputFile9a = 'Hood/Pharyngeal/1create_snake/curve.exnode';
outputFile9='lengths1/hoodorallength.txt';
outputFile9a='lengths1/hoodpharyngeallength.txt';
outputFile9b='lengths1/hoodtotallength.txt';

inputFile10 = 'Hud/Oral/1create_snake/curve.exnode';
inputFile10a = 'Hud/Pharyngeal/1create_snake/curve.exnode';
outputFile10='lengths1/hudorallength.txt';
outputFile10a='lengths1/hudpharyngeallength.txt';
outputFile10b='lengths1/hudtotallength.txt';

inputFile11 = 'Whod/Oral/1create_snake/curve.exnode';
inputFile11a = 'Whod/Pharyngeal/1create_snake/curve.exnode';
outputFile11='lengths1/whodorallength.txt';
outputFile11a='lengths1/whodpharyngeallength.txt';
outputFile11b='lengths1/whodtotallength.txt';

%Area calculation 
inputArea1 = 'Had/Oral/7areacalculation/area.txt';
inputArea1a = 'Had/Pharyngeal/7areacalculation/area.txt';

inputArea2 = 'Hard/Oral/7areacalculation/area.txt';
inputArea2a = 'Hard/Pharyngeal/7areacalculation/area.txt';

inputArea3 = 'Head/Oral/7areacalculation/area.txt';
inputArea3a = 'Head/Pharyngeal/7areacalculation/area.txt';

inputArea4 = 'Heed/Oral/7areacalculation/area.txt';
inputArea4a = 'Heed/Pharyngeal/7areacalculation/area.txt';

inputArea5 = 'Herd/Oral/7areacalculation/area.txt';
inputArea5a = 'Herd/Pharyngeal/7areacalculation/area.txt';

inputArea6 = 'Hid/Oral/7areacalculation/area.txt';
inputArea6a = 'Hid/Pharyngeal/7areacalculation/area.txt';

inputArea7 = 'Hoard/Oral/7areacalculation/area.txt';
inputArea7a = 'Hoard/Pharyngeal/7areacalculation/area.txt';

inputArea8 = 'Hod/Oral/7areacalculation/area.txt';
inputArea8a = 'Hod/Pharyngeal/7areacalculation/area.txt';

inputArea9 = 'Hood/Oral/7areacalculation/area.txt';
inputArea9a = 'Hood/Pharyngeal/7areacalculation/area.txt';

inputArea10 = 'Hud/Oral/7areacalculation/area.txt';
inputArea10a = 'Hud/Pharyngeal/7areacalculation/area.txt';

inputArea11 = 'Whod/Oral/7areacalculation/area.txt';
inputArea11a = 'Whod/Pharyngeal/7areacalculation/area.txt';


volumeoutput=zeros(11,6)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Had %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength1]=orallengthcalc(inputFile1,outputFile1);
[plength1]=pharyngeallengthcalc(inputFile1a,outputFile1a);
[length1]=totallengthcalc(outputFile1,outputFile1a,outputFile1b);

%Area Calculation
[hadarea,oarea1,parea1]=areacalc(inputArea1,inputArea1a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length1';
A(:,2)=hadarea';
fid = fopen('distance_area/had.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
had_o_vol=trapz(olength1, oarea1);
had_p_vol=trapz(plength1,parea1);
had_tot_vol=trapz(length1,hadarea);
volumeoutput(1,1)=had_o_vol
volumeoutput(1,2)=had_p_vol
volumeoutput(1,3)=had_tot_vol
had_error=had_tot_vol-(had_o_vol+had_p_vol);
volumeoutput(1,4)=had_error
opercent=had_o_vol/had_tot_vol*100
volumeoutput(1,5)=opercent;
ppercent=had_p_vol/had_tot_vol*100
volumeoutput(1,6)=ppercent;


%Plot
%Smooth the lines
[length1Smooth,area1Smooth]= smoothLine(length1,hadarea);
subplot(6,9,30), plot(length1Smooth, area1Smooth,'b-','LineWidth',1.5)

%Velar Port
line([length1(15) length1(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Had');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)
 


%%%%%%%%%%%%%%%%%%%%%%%%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hard %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength2]=orallengthcalc(inputFile2,outputFile2);
[plength2]=pharyngeallengthcalc(inputFile2a,outputFile2a);
[length2]=totallengthcalc(outputFile2,outputFile2a,outputFile2b);

%Area Calculation
[hardarea,oarea2,parea2]=areacalc(inputArea2,inputArea2a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length2';
A(:,2)=hardarea';
fid = fopen('distance_area/hard.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
hard_o_vol=trapz(olength2, oarea2);
hard_p_vol=trapz(plength2,parea2);
hard_tot_vol=trapz(length2,hardarea);
volumeoutput(2,1)=hard_o_vol
volumeoutput(2,2)=hard_p_vol
volumeoutput(2,3)=hard_tot_vol
hard_error=hard_tot_vol-(hard_o_vol+hard_p_vol);
volumeoutput(2,4)=hard_error
opercent=hard_o_vol/hard_tot_vol*100
volumeoutput(2,5)=opercent;
ppercent=hard_p_vol/hard_tot_vol*100
volumeoutput(2,6)=ppercent;

%Plot
%Smooth the lines
[length2Smooth,area2Smooth]= smoothLine(length2,hardarea);
subplot(6,9,50), plot(length2Smooth, area2Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length2(15) length2(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Hard');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Head %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength3]=orallengthcalc(inputFile3,outputFile3);
[plength3]=pharyngeallengthcalc(inputFile3a,outputFile3a);
[length3]=totallengthcalc(outputFile3,outputFile3a,outputFile3b);

%Area Calculation
[headarea,oarea3,parea3]=areacalc(inputArea3,inputArea3a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length3';
A(:,2)=headarea';
fid = fopen('distance_area/head.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
head_o_vol=trapz(olength3, oarea3);
head_p_vol=trapz(plength3,parea3);
head_tot_vol=trapz(length3,headarea);
volumeoutput(3,1)=head_o_vol
volumeoutput(3,2)=head_p_vol
volumeoutput(3,3)=head_tot_vol
head_error=head_tot_vol-(head_o_vol+head_p_vol);
volumeoutput(3,4)=head_error
opercent=head_o_vol/head_tot_vol*100
volumeoutput(3,5)=opercent;
ppercent=head_p_vol/head_tot_vol*100
volumeoutput(3,6)=ppercent;

%Plot
%Smooth the lines
[length3Smooth,area3Smooth]= smoothLine(length1,headarea);
subplot(6,9,20), plot(length3Smooth, area3Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length3(15) length3(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Head');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Heed %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength4]=orallengthcalc(inputFile4,outputFile4);
[plength4]=pharyngeallengthcalc(inputFile4a,outputFile4a);
[length4]=totallengthcalc(outputFile4,outputFile4a,outputFile4b);

%Area Calculation
[heedarea,oarea4,parea4]=areacalc(inputArea4,inputArea4a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length4';
A(:,2)=heedarea';
fid = fopen('distance_area/heed.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
heed_o_vol=trapz(olength4, oarea4);
heed_p_vol=trapz(plength4,parea4);
heed_tot_vol=trapz(length4,heedarea);
volumeoutput(4,1)=heed_o_vol
volumeoutput(4,2)=heed_p_vol
volumeoutput(4,3)=heed_tot_vol
heed_error=heed_tot_vol-(heed_o_vol+heed_p_vol);
volumeoutput(4,4)=heed_error
opercent=heed_o_vol/heed_tot_vol*100
volumeoutput(4,5)=opercent;
ppercent=heed_p_vol/heed_tot_vol*100
volumeoutput(4,6)=ppercent;

%Plot
%Smooth the lines
[length4Smooth,area4Smooth]= smoothLine(length4,heedarea);
subplot(6,9,1), plot(length4Smooth, area4Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length4(15) length4(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Heed');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Herd %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength5]=orallengthcalc(inputFile5,outputFile5);
[plength5]=pharyngeallengthcalc(inputFile5a,outputFile5a);
[length5]=totallengthcalc(outputFile5,outputFile5a,outputFile5b);

%Area Calculation
[herdarea,oarea5,parea5]=areacalc(inputArea5,inputArea5a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length5';
A(:,2)=herdarea';
fid = fopen('distance_area/herd.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
herd_o_vol=trapz(olength5, oarea5);
herd_p_vol=trapz(plength5,parea5);
herd_tot_vol=trapz(length5,herdarea);
volumeoutput(5,1)=herd_o_vol
volumeoutput(5,2)=herd_p_vol
volumeoutput(5,3)=herd_tot_vol
herd_error=herd_tot_vol-(herd_o_vol+herd_p_vol);
volumeoutput(5,4)=herd_error
opercent=herd_o_vol/herd_tot_vol*100
volumeoutput(5,5)=opercent;
ppercent=herd_p_vol/herd_tot_vol*100
volumeoutput(5,6)=ppercent;

%Plot
%Smooth the lines
[length5Smooth,area5Smooth]= smoothLine(length5,herdarea);
subplot(6,9,22), plot(length5Smooth, area5Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length5(15) length5(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Herd')
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hid %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength6]=orallengthcalc(inputFile6,outputFile6);
[plength6]=pharyngeallengthcalc(inputFile6a,outputFile6a);
[length6]=totallengthcalc(outputFile6,outputFile6a,outputFile6b);

%Area Calculation
[hidarea,oarea6,parea6]=areacalc(inputArea6,inputArea6a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length6';
A(:,2)=hidarea';
fid = fopen('distance_area/hid.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
hid_o_vol=trapz(olength6, oarea6);
hid_p_vol=trapz(plength6,parea6);
hid_tot_vol=trapz(length6,hidarea);
volumeoutput(6,1)=hid_o_vol
volumeoutput(6,2)=hid_p_vol
volumeoutput(6,3)=hid_tot_vol
hid_error=hid_tot_vol-(hid_o_vol+hid_p_vol);
volumeoutput(6,4)=hid_error
opercent=hid_o_vol/hid_tot_vol*100
volumeoutput(6,5)=opercent;
ppercent=hid_p_vol/hid_tot_vol*100
volumeoutput(6,6)=ppercent;

%Plot
%Smooth the lines
[length6Smooth,area6Smooth]= smoothLine(length6,hidarea);
subplot(6,9,12), plot(length6Smooth, area6Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length6(15) length6(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Hid');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hoard %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength7]=orallengthcalc(inputFile7,outputFile7);
[plength7]=pharyngeallengthcalc(inputFile7a,outputFile7a);
[length7]=totallengthcalc(outputFile7,outputFile7a,outputFile7b);

%Area Calculation
[hoardarea,oarea7,parea7]=areacalc(inputArea7,inputArea7a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length7';
A(:,2)=hoardarea';
fid = fopen('distance_area/hoard.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
hoard_o_vol=trapz(olength7, oarea7);
hoard_p_vol=trapz(plength7,parea7);
hoard_tot_vol=trapz(length7,hoardarea);
volumeoutput(7,1)=hoard_o_vol
volumeoutput(7,2)=hoard_p_vol
volumeoutput(7,3)=hoard_tot_vol
hoard_error=hoard_tot_vol-(hoard_o_vol+hoard_p_vol);
volumeoutput(7,4)=hoard_error
opercent=hoard_o_vol/hoard_tot_vol*100
volumeoutput(7,5)=opercent;
ppercent=hoard_p_vol/hoard_tot_vol*100
volumeoutput(7,6)=ppercent;

%Plot
%Smooth the lines
[length7Smooth,area7Smooth]= smoothLine(length7,hoardarea);
subplot(6,9,8), plot(length7Smooth, area7Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length7(15) length7(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Hoard');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hod %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength8]=orallengthcalc(inputFile8,outputFile8);
[plength8]=pharyngeallengthcalc(inputFile8a,outputFile8a);
[length8]=totallengthcalc(outputFile8,outputFile8a,outputFile8b);

%Area Calculation
[hodarea,oarea8,parea8]=areacalc(inputArea8,inputArea8a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length8';
A(:,2)=hodarea';
fid = fopen('distance_area/hod.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
hod_o_vol=trapz(olength8, oarea8);
hod_p_vol=trapz(plength8,parea8);
hod_tot_vol=trapz(length8,hodarea);
volumeoutput(8,1)=hod_o_vol
volumeoutput(8,2)=hod_p_vol
volumeoutput(8,3)=hod_tot_vol
hod_error=hod_tot_vol-(hod_o_vol+hod_p_vol);
volumeoutput(8,4)=hod_error
opercent=hod_o_vol/hod_tot_vol*100;
volumeoutput(8,5)=opercent
ppercent=hod_p_vol/hod_tot_vol*100;
volumeoutput(8,6)=ppercent

%Plot
%Smooth the lines
[length8Smooth,area8Smooth]= smoothLine(length8,hodarea);
subplot(6,9,35), plot(length8Smooth, area8Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length8(15) length8(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Hod');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %% Hood %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %Length Calculation
% [olength9]=orallengthcalc(inputFile9,outputFile9);
% [plength9]=pharyngeallengthcalc(inputFile9a,outputFile9a);
% [length9]=totallengthcalc(outputFile9,outputFile9a,outputFile9b);
% 
% %Area Calculation
% [hoodarea,orea9,parea9]=areacalc(inputArea9,inputArea9a);
% 
% %Write to text file for formant plot
% A=zeros(29,2);
% A(:,1)=length9';
% A(:,2)=hoodarea';
% fid = fopen('distance_area/hood.txt','w');
% if( fid == -1)
%     disp('Error opening the output file');
% else
%     fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
%     st = fclose(fid);
% end

% %Volume
% hood_o_vol=trapz(olength9, oarea9);
% hood_p_vol=trapz(plength9,parea9);
% hood_tot_vol=trapz(length9,hoodarea);
% volumeoutput(9,1)=hood_o_vol
% volumeoutput(9,2)=hood_p_vol
% volumeoutput(9,3)=hood_tot_vol
% hood_error=hood_tot_vol-(hood_o_vol+hood_p_vol);
% volumeoutput(9,4)=hood_error
% opercent=hood_o_vol/hood_tot_vol*100;
% volumeoutput(9,5)=opercent
% ppercent=hood_p_vol/hood_tot_vol*100;
% volumeoutput(9,6)=ppercent

% %Plot
% %Smooth the lines
% [length9Smooth,area9Smooth]= smoothLine(length9,hoodarea);
% subplot(6,9,16), plot(length9Smooth, area9Smooth,'b-','LineWidth',1.5)
% 
%Velar Port
% line([length9(15) length9(15)],[0 1000]);
% xlim([0 200])
% ylim([0 1000])
% title('Hood');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hud %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength10]=orallengthcalc(inputFile10,outputFile10);
[plength10]=pharyngeallengthcalc(inputFile10a,outputFile10a);
[length10]=totallengthcalc(outputFile10,outputFile10a,outputFile10b);

%Area Calculation
[hudarea,oarea10,parea10]=areacalc(inputArea10,inputArea10a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length10';
A(:,2)=hudarea';
fid = fopen('distance_area/hud.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
hud_o_vol=trapz(olength10, oarea10);
hud_p_vol=trapz(plength10,parea10);
hud_tot_vol=trapz(length10,hudarea);
volumeoutput(10,1)=hud_o_vol
volumeoutput(10,2)=hud_p_vol
volumeoutput(10,3)=hud_tot_vol
hud_error=hud_tot_vol-(hud_o_vol+hud_p_vol);
volumeoutput(10,4)=hud_error
opercent=hud_o_vol/hud_tot_vol*100;
volumeoutput(10,5)=opercent
ppercent=hud_p_vol/hud_tot_vol*100;
volumeoutput(10,6)=ppercent

%Plot
%Smooth the lines
[length10Smooth,area10Smooth]= smoothLine(length10,hudarea);
subplot(6,9,41), plot(length8Smooth, area8Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length10(15) length10(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Hud');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Who'd %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Length Calculation
[olength11]=orallengthcalc(inputFile11,outputFile11);
[plength11]=pharyngeallengthcalc(inputFile11a,outputFile11a);
[length11]=totallengthcalc(outputFile11,outputFile11a,outputFile11b);

%Area Calculation
[whodarea,oarea11,parea11]=areacalc(inputArea11,inputArea11a);

%Write to text file for formant plot
A=zeros(29,2);
A(:,1)=length11';
A(:,2)=whodarea';
fid = fopen('distance_area/whod.txt','w');
if( fid == -1)
    disp('Error opening the output file');
else
    fprintf(fid, [repmat('%f ', 1, size(A,2)), '\n'], A')
    st = fclose(fid);
end

%Volume
whod_o_vol=trapz(olength11, oarea11);
whod_p_vol=trapz(plength11,parea11);
whod_tot_vol=trapz(length11,whodarea);
volumeoutput(11,1)=whod_o_vol
volumeoutput(11,2)=whod_p_vol
volumeoutput(11,3)=whod_tot_vol
whod_error=whod_tot_vol-(whod_o_vol+whod_p_vol);
volumeoutput(11,4)=whod_error
opercent=whod_o_vol/whod_tot_vol*100;
volumeoutput(11,5)=opercent
ppercent=whod_p_vol/whod_tot_vol*100;
volumeoutput(11,6)=ppercent

%Plot
%Smooth the lines
[length11Smooth,area11Smooth]= smoothLine(length11,whodarea);
subplot(6,9,4), plot(length11Smooth, area11Smooth,'b-','LineWidth',1.5)
%Velar Port
line([length11(15) length11(15)],[0 1000]);
xlim([0 200])
ylim([0 1000])
title('Whod');
% text(10,750,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,750,['%=', num2str(ppercent','%11.3g')],'FontSize',8)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Overall Plot %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Overall Plot Titles
p=mtit('VT01','fontsize',14,'color',[1 0 0],'xoff',-.001,'yoff',.025);

[ax,h1]=suplabel('VT Length (mm)', 'x');
[ax,h2]=suplabel('Crossectional Area (mm^2)','y');

%% Volume
had_int=trapz(length1Smooth, area1Smooth);
hard=trapz(length2Smooth, area2Smooth);
head=trapz(length3Smooth, area3Smooth);
heed=trapz(length4Smooth, area4Smooth);
herd=trapz(length5Smooth, area5Smooth);
hid=trapz(length6Smooth, area6Smooth);
hoard=trapz(length7Smooth, area7Smooth);
hod=trapz(length8Smooth, area8Smooth);
%hood=trapz(length9Smooth, area9Smooth);
hud=trapz(length10Smooth, area10Smooth);
whod=trapz(length11Smooth, area11Smooth);

