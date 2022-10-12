%PSD estimates of random noise

fs = 2^8; %sampling frequency in Hz
sig = 6; %std of normal (white) noise with zero mean

dt = 1/fs; 
Ttot = 2; %total measurement time
N = Ttot*fs; %(number of points

%noise time series
t = (0:1/fs:Ttot); %time in seconds
n = sqrt(sig)*randn(size(t));

%one fft
Sh = (2/(fs*N))*abs(fft(n)).^2;
f = (0:1/Ttot:fs);
z1 = std(Sh)

%two ffts
f2 = (0:2/Ttot:fs-1);
Sh2 = 0*f2;
for jj=1:2
    ix = ((jj-1)*N/2+1:jj*N/2);
    s = (4/(fs*N))*abs(fft(n(ix))).^2;
    Sh2 = Sh2 + s;
end
Sh2 = Sh2/2;
z2 = std(Sh2)

%four ffts
f4 = (0:4/Ttot:fs-1);
Sh4 = 0*f4;
for jj=1:4
    ix = ((jj-1)*N/4+1:jj*N/4);
    s = (8/(fs*N))*abs(fft(n(ix))).^2;
    Sh4 = Sh4 + s;
end
Sh4 = Sh4/4;
z4 = std(Sh4)

%eight ffts
f8 = (0:8/Ttot:fs-1);
Sh8 = 0*f8;
for jj=1:8
    ix = ((jj-1)*N/8+1:jj*N/8);
    s = (16/(fs*N))*abs(fft(n(ix))).^2;
    Sh8 = Sh8 + s;
end
Sh8 = Sh8/8;
z8 = std(Sh8)


figure(1)
plot(f, Sh, f2, Sh2, f4, Sh4,'k.-', f8, Sh8,'r*-')
## legend(['std = ' num2str(z1,2)],...
##     ['std = ' num2str(z2,2)],...
##     ['std = ' num2str(z4,2)],...
##     ['std = ' num2str(z8,2)],fontsize=14)
xlim([0, fs])
xlabel('freq(Hz)')
ylabel('Power Spectral Density')
grid on
shg
