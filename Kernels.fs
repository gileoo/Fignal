module Kernels

module Ranged =

    let ExpDiff (shift) (a) (T1) (T2) (t:float[]) =
    
        let ac = max 0.0 a
        let ampScaled = 
            if a > 0.0 then
                let mx = T1 * T2 * (log (T1/T2))  / (T1 - T2)
                let ma = exp (-mx/T2) - exp (-mx/T1) |> abs
                ac / ma
            else
                let meanDt = 
                    Array.zeroCreate (t.Length-1)
                    |> Array.mapi( fun i x -> t.[i+1] - t.[i] )
                    |> Array.average
                1.0 / (T2-T1) / meanDt

        printf "ampScale: %f" ampScaled

        if T1 > 0.0 then
            t
            |> Array.map( (-) shift)
            |> Array.map( fun x -> ampScaled * ( exp (-x/T2) - exp (-x/T1) ) )
        else
            t
            |> Array.map( (-) shift)
            |> Array.map( fun x -> ampScaled * ( exp (-x/T2) ) )
        
    

(*
function conductance = bateman(time,onset,amp,tau1,tau2)

if tau1 < 0 || tau2 < 0
    error('tau1 or tau2 < 0: (%f, %f)\n', tau1, tau2);
end

if tau1 == tau2
    error('tau1 == tau2 == %f', tau1);
end

conductance = zeros(size(time));
range = find(time > onset);
if isempty(range);
    return;
end
xr = time(range) - onset;


if amp > 0
    maxx = tau1 * tau2 * log(tau1/tau2) / (tau1 - tau2);  %b' = 0
    maxamp = abs(exp(-maxx/tau2) - exp(-maxx/tau1));
    c =  amp/maxamp;

else %amp == 0: normalized bateman, area(bateman) = 1/sr
    sr = round(1/mean(diff(time)));
    c = 1/((tau2 - tau1) * sr);

end

if tau1 > 0
    conductance(range) = c * (exp(-xr/tau2) - exp(-xr/tau1));
else
    conductance(range) = c * exp(-xr/tau2);
end












function component = bateman_gauss(time, onset, amp, tau1, tau2, sigma)

component = bateman(time,onset,0,tau1,tau2);

if sigma > 0
    sr = round(1/mean(diff(time)));
    winwidth2 = ceil(sr*sigma*4); %round half winwidth: 4 SD to each side
    t = 1:(winwidth2*2+1); %odd number (2*winwidth-half+1)
    g = normpdf(t, winwidth2+1, sigma*sr);
    g = g / max(g) * amp;
    bg = conv([ones(1,winwidth2)*component(1), component, ones(1,winwidth2)*component(end)], g);
    
    component = bg((winwidth2*2+1) : (end-winwidth2*2));

end
*)