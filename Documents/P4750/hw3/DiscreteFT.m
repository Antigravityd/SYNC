%Discrete Fourier transform
%Plot DFT of a sine wave for different values of fs


t = linspace(-3,3, 1e4);
f0 = 2;
phi = 0.2;
T = 3;

p = 0;
for ii = 1:6
    fs = 1.1*f0*ii;
    td = (-T:1/fs:T);
    s = sin(2*pi*f0*t+phi);
    sd = sin(2*pi*f0*td+phi);
    
    w = 2*pi*(0:1/T:fs);
    F = 0*w;
    for jj=1:length(w)
        F(jj) = sum(sd.*exp(-1i*w(jj)*td));
    end
       
    subplot(6,2,p+1)
    plot(t,s)
    hold on
    h = plot(td, sd,'.-');
    h.MarkerSize = 10 ;
    hold off
    grid on

    subplot(6,2,p+2)
    plot(w/(2*pi),abs(F),'.-')
    title(['fs = ' num2str(fs,2) ' Hz'])
    grid on

    p= p+2;

end

subplot(6,2,p-1)
xlabel('time(s)')

subplot(6,2,p)
xlabel('freq(Hz)')

shg