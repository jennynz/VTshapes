volumeoutput

subplot(6,9,31) % a small box is created, this is the axes to draw in
title('Had')
opercent=volumeoutput(1,5);
ppercent=volumeoutput(1,6);
xlim([0 200])
ylim([0 200])
line([length1(15) length1(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length1(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length1(29)','%11.3g')],'FontSize',8)
axis off
saveas(gcf,'hadvol.fig') 

%Image import
subplot(6,9,51) % a small box is created, this is the axes to draw in
title('Hard')
opercent=volumeoutput(2,5);
ppercent=volumeoutput(2,6);
xlim([0 200])
ylim([0 200])
line([length2(15) length2(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length2(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length2(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'hardvol.fig') 


subplot(6,9,21) % a small box is created, this is the axes to draw in
title('Head')
opercent=volumeoutput(3,5);
ppercent=volumeoutput(3,6);
xlim([0 200])
ylim([0 200])
line([length3(15) length3(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length3(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length3(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'headvol.fig') 

%Image import
subplot(6,9,2) % a small box is created, this is the axes to draw in
title('Heed')
opercent=volumeoutput(4,5);
ppercent=volumeoutput(4,6);
xlim([0 200])
ylim([0 200])
line([length4(15) length4(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length4(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length4(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'heedvol.fig') 

%Image import
subplot(6,9,23) % a small box is created, this is the axes to draw in
title('Herd')
opercent=volumeoutput(5,5);
ppercent=volumeoutput(5,6);
xlim([0 200])
ylim([0 200])
line([length5(15) length5(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length5(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length5(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'herdvol.fig') 

%Image import
subplot(6,9,13) % a small box is created, this is the axes to draw in
title('Hid')
opercent=volumeoutput(6,5);
ppercent=volumeoutput(6,6);
xlim([0 200])
ylim([0 200])
line([length6(15) length6(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length6(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length6(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'hidvol.fig') 


subplot(6,9,9) % a small box is created, this is the axes to draw in
title('Hoard')
opercent=volumeoutput(7,5);
ppercent=volumeoutput(7,6);
xlim([0 200])
ylim([0 200])
line([length7(15) length7(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length7(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length7(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'hoardvol.fig') 


subplot(6,9,36) % a small box is created, this is the axes to draw in
title('Hod')
opercent=volumeoutput(8,5);
ppercent=volumeoutput(8,6);
xlim([0 200])
ylim([0 200])
line([length8(15) length8(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length8(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length8(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'hod.fig') 

% 
% subplot(6,9,17) % a small box is created, this is the axes to draw in
% title('Hood')
% opercent=volumeoutput(9,5);
% ppercent=volumeoutput(9,6);
% xlim([0 200])
% ylim([0 200])
% line([length9(15) length9(15)],[0 1000]);
% text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
% text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
% text(2,50,['oral=',num2str(length9(15)','%11.3g')],'FontSize',8)
% text(120,50,['total=',num2str(length9(29)','%11.3g')],'FontSize',8)
% axis off % make sure no axis labels present
% saveas(gcf,'hoodvol.fig') 

%Image import
subplot(6,9,42) % a small box is created, this is the axes to draw in
title('Hud')
opercent=volumeoutput(10,5);
ppercent=volumeoutput(10,6);
xlim([0 200])
ylim([0 200])
line([length10(15) length10(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length10(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length10(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'hudvol.fig') 

%Image import
subplot(6,9,5) % a small box is created, this is the axes to draw in
title('Whod')
opercent=volumeoutput(11,5);
ppercent=volumeoutput(11,6);
xlim([0 200])
ylim([0 200])
line([length11(15) length11(15)],[0 1000]);
text(2,150,[' %= ',num2str(opercent','%11.3g')],'FontSize',8)
text(120,150,['% =', num2str(ppercent','%11.3g')],'FontSize',8)
text(2,50,['oral=',num2str(length11(15)','%11.3g')],'FontSize',8)
text(120,50,['total=',num2str(length11(29)','%11.3g')],'FontSize',8)
axis off % make sure no axis labels present
saveas(gcf,'whodvol.fig') 

%Overall Plot Titles
p=mtit('VT01 Volume(%) and Length (mm)','fontsize',14,'color',[1 0 0],'xoff',-.001,'yoff',.025);
