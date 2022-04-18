;
; (c)MX^Addict
;

%define PUBLIC_FN(n,i) _ %+ n %+@ %+i
%define PUBLIC_DATA(n) _ %+ n

%define SECT_BSS(n)    section n bss
%define SECT_DATA(n)   section n data
%define SECT_RDATA(n)  section n rdata
%define SECT_TEXT(n)   section n code
