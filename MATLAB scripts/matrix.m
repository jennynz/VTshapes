function [oral, pharyngeal,total,smooth_total]=matrix(vocaltractnumber,vowelnumber)
oral=zeros(7,11)
oral(vocaltractnumber,vowelnumber)=
pharyngeal=zeros(7,11)
total=zeros(7,11)
smooth_total=zeros(7,11)
% oral_norm=zeros(8,12)
% pharyngeal_norm=zeros(8,12)