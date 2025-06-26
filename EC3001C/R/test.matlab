% Function 2: Simulate data using the Single-factor Model/the Hybrid Model                         
function[WE,ICM,ICM2,TaxRate]=fit_sim(W0,M1,M2,M3,M4,Y,nper,C0,C1,beta1,sigma,growth,tax,allowance,gamma,tmseed)
% NEW INPUT----------------------------------------------------------------
% M1:Labour model, M2:Growth model, M3:Tax model, M4:Capital model
% Mi can take on two values, "1" means being considered while "0" means not

% W0:        Initial wealth at the begining of period 1
% nper:      Number of periods
% c0:        Individual constant consumption level (lower bound)
% c1:        Proportional consumption out of individual income
% edr:       Mrginal return in terms of human capital
% growth:    A net exogenous growth rate
% cpt:       Consumption tax rate
% kpt:       Capital tax rate
% OUTPUT ------------------------------------------------------------------
% WE:        Individual Wealth at the end of each period (npel by nper)
% ICM:       Individual total income in each period
npel = size(W0,1);         % npel: Number of people
WB   = zeros(npel,nper+1); % WB: Wealth at the beginning of each period
WE   = zeros(npel,nper);
CSM  = zeros(npel,nper); % Individual consumption if consumption tax is in
CSM0 = zeros(npel,nper); % Individual consumption if consumption tax is out
ICM  = zeros(npel,nper); % Individual income before any subsidy or transfer
ICM2 = zeros(npel,nper); % Individual total income including transfer
WB(:,1)   = W0(:,1);
rawprob   = zeros(npel,1);
inv_share = zeros(npel,1);
TaxRate   = zeros(nper,1);

Tax_expmt = 0;  % Taxation experiment trigger,
          % 1: income tax is replaced by wealth tax; 2: by consumption tax

rng(tmseed);  % Fix the time seed for random generator (this syntax is newest)
if M1==1 
   effort = normrnd(0,sigma,npel,1); 
else
   effort = zeros(npel,1);
end
if M2==1
   G = growth;
else
   G = 0.0;
end
for j=1:nper  
    if M3==1
       T = tax;
    else
       T = 0.0;
    end    
% Creates probabilities determined by Human Capital
    rawprob(:,1) = 1 - 1./(ones(npel,1)+exp(ones(npel,1)*log(1/(npel-1))+beta1*effort));
    prob1 = rawprob/sum(rawprob); % Logit probability
% Creates probabilities determined by Physical Capital Investment
    if j==1
       inv_share(:,1) = 1/npel;          % No investment effect in period 1
    else
       inv_share(:,1) = WE(:,j-1)/sum(WE(:,j-1),1);
    end
%------2nd, Distribute GDP sequentially
    y1 = zeros(npel,1);                       % Initial income distribution
    if M4==0
    gain0 = randsrc(npel,1,[1:1:npel; prob1']);       % Probability of labour income
       for i=1:npel
           y1(gain0(i,1),1) = y1(gain0(i,1),1) + (Y*(1+G)^(j-1))/npel;         
       end       
    elseif M4==1 && M1==0
       gain1 = randsrc(npel,1,[1:1:npel; inv_share']); % Probability of capital income
       for i=1:npel
           y1(gain1(i,1),1) = y1(gain1(i,1),1) + (Y*(1+G)^(j-1))/npel;         
       end
    elseif M1==1 && M4==1
       Lpower = gamma;
       gain0 = randsrc(npel,1,[1:1:npel; prob1']);     % Probability of labour income
       for i=1:npel                        % 1st, distribute labour income
           y1(gain0(i,1),1) = y1(gain0(i,1),1) + Lpower*(Y*(1+G)^(j-1))/npel;         
       end
       gain1 = randsrc(npel,1,[1:1:npel; inv_share']); % Probability of capital income
       for i=1:npel                        % 2nd, distribute capital income
           y1(gain1(i,1),1) = y1(gain1(i,1),1) + (1.0-Lpower)*(Y*(1+G)^(j-1))/npel;         
       end       
    end
%------3rd, Calculate income tax burden rate if there is no other taxes
    taxable_y = zeros(npel,1);
    for i=1:npel
        if y1(i,1)>allowance*(Y*(1+G)^(j-1)/npel)
           taxable_y(i,1) = y1(i,1) - allowance*(Y*(1+G)^(j-1)/npel);                
        end
    end
    clear taxpay_y0;
    taxpay_y0 = taxable_y*T;
    T_burden = sum(taxpay_y0);   
%=======Case1: Only income tax    
    if Tax_expmt==0
       taxpay_y = taxpay_y0;
       Taxall_y = T_burden;
     % Subsidy to the very poor
       y2 = zeros(npel,1);          % 1st time transfer
       for i=1:npel
           if WB(i,j) + y1(i,1) - taxpay_y(i,1) < C0*(1+G)^(j-1)             
              y2(i,1) = C0*(1+G)^(j-1);
           end
       end
       num_sub1 = sum(y2(:,1));     % Total amount of the 1st time transfer
       ICM2(:,j) = y1 - taxpay_y;   % Income before transfer
     % Transfer the rest income tax revenue to agents identically
       y3 = zeros(npel,1);          % 2nd time transfer
       for i=1:npel
           if Taxall_y>num_sub1
              y3(i,1) = (Taxall_y-num_sub1)/npel;              
           end
       end
       ICM(:,j) = ICM2(:,j) + y2 + y3;               % Income after 2nd transfer       
     % Consumption according to "ICM2"
       for i=1:npel
           if WB(i,j) + ICM2(i,j) < C0*(1+G)^(j-1) + C1*ICM2(i,j)
              CSM(i,j) = WB(i,j) + ICM2(i,j);
           else
              CSM(i,j) = C0*(1+G)^(j-1) + C1*ICM2(i,j);
           end
       end        
     % Calculate individual wealth after consumption
       WE(:,j) = WB(:,j) + ICM(:,j) - CSM(:,j);
       TaxRate(j,1) = T;       
%=======Case2: Only wealth tax but with same tax burden as income tax 
    elseif Tax_expmt==1       
       TW_multp = 1;       
       WB_b1 = WB(:,j) + y1;
       Wmean = mean(WB_b1);
      % Wmedian = median(WB_b1);
       taxable_k = zeros(npel,1);           % Taxable wealth for each agent
       for i=1:npel
           if WB_b1(i,1) > TW_multp*Wmean
              taxable_k(i,1) = WB_b1(i,1) - TW_multp*Wmean;
          % if WB_b1(i,1)>TW_multp*Wmedian
          %    taxable_k(i,1) = WB_b1(i,1)-TW_multp*Wmedian;
          % if WB_b1(i,1)>TW_multp*Wtop(round((1.0-0.05)*npel),1)
          %    taxable_k(i,1) = WB_b1(i,1)-TW_multp*Wtop(round((1.0-0.05)*npel),1);
           end
       end       
       tau_k = T_burden/sum(taxable_k(:,1));
       TaxRate(j,1) = tau_k;
       clear taxpay_k;   
       taxpay_k = taxable_k*tau_k;  
       Taxall_k = sum(taxpay_k(:,1));
       WB_b2 = WB_b1 - taxpay_k;
     % Subsidy to the very poor
       y2 = zeros(npel,1);
       for i=1:npel
           if WB_b2(i,1)<C0*(1+G)^(j-1)
              y2(i,1) = C0*(1+G)^(j-1);
           end
       end        
       num_sub2 = sum(y2);               % Total amount of the 1st transfer
       WB_b3 = WB_b2 + y2;               % Wealth after 1st transfer
       ICM2(:,j) = y1;                   % Income before transfer       
     % Transfer the rest income tax revenue to agents identically
       y3 = zeros(npel,1);               % 2nd time transfer
       for i=1:npel
           if Taxall_k>num_sub2
              y3(i,1) = (Taxall_k-num_sub2)/npel;
           end
       end
       WB_b4 = WB_b3 + y3;                      % Wealth after 2nd transfer        
       ICM(:,j) = ICM2(:,j) + y2 + y3;          % Income after 2nd transfer       
     % Consumption according to "ICM2" and "WB_b3"
       for i=1:npel
           if WB_b3(i,1)<C0*(1+G)^(j-1)+C1*ICM2(i,j)
              CSM(i,j) = WB_b3(i,1);
           else
              CSM(i,j) = C0*(1+G)^(j-1) + C1*ICM2(i,j);
           end
       end        
     % Calculate individual wealth after consumption
       WE(:,j) = WB_b4 - CSM(:,j);
%=======Case3: Only consumption tax but with same tax burden as income tax 
    elseif Tax_expmt==2             
     % Note: We have to transfer subsidy by public deficit first and then
     %       balance the deficit by consumption tax revenue in thise case.
       WB_c1 = WB(:,j) + y1;
     % Subsidy to the very poor
       y2 = zeros(npel,1);
       for i=1:npel
           if WB_c1(i,1)<C0*(1+G)^(j-1)
              y2(i,1) = C0*(1+G)^(j-1);
           end
       end        
       num_sub3 = sum(y2);               % Total amount of the 1st transfer        
       WB_c2 = WB_c1 + y2;               % Wealth after 1st transfer
       ICM2(:,j) = y1;                   % Income after before transfer       
%----- Following steps aim to compute the proper consumption tax rate        
       % Step1. Calculate initial tau_c given the income tax burden
       %       (Assume agents consume following the consumption equation)
       tau_c0 = T_burden/sum(C1*ICM2(:,j),1);
       Tburden_tol = 0.01;  % Tolerance level of solving for tau_c
       % Step2. Calculate the real consumption tax burden given tau_c0
       for i=1:npel
           if WB_c2(i,1) < C0*(1+G)^(j-1) + C1*ICM2(i,j)*(1+tau_c0)
              CSM(i,j) = WB_c2(i,1);           % Consumption with tau_c0         
           else
              CSM(i,j) = C0*(1+G)^(j-1) + C1*ICM2(i,j)*(1+tau_c0);
           end
           if WB_c2(i,1) < C0*(1+G)^(j-1) + C1*ICM2(i,j)
              CSM0(i,j) = WB_c2(i,1);          % Consumption without tau_c0           
           else
              CSM0(i,j) = C0*(1+G)^(j-1) + C1*ICM2(i,j);
           end
       end
       Tburden_diff = (sum(CSM(:,j),1)-sum(CSM0(:,j),1))/T_burden;
       tau_c = tau_c0;
       while abs(Tburden_diff-1.0) > Tburden_tol
           if Tburden_diff-1.0>Tburden_tol             % tau_c is too large
              tau_c = (tau_c0+tau_c)/2;
           elseif Tburden_diff-1.0<-Tburden_tol        % tau_c is too small
              tau_c = tau_c*(1+0.1);
           end
           for i=1:npel      % Recompute consumption tax burden given tau_c
               if WB_c2(i,1) < C0*(1+G)^(j-1) + C1*ICM2(i,j)*(1+tau_c)
                  CSM(i,j) = WB_c2(i,1);       
               else
                  CSM(i,j) = C0*(1+G)^(j-1) + C1*ICM2(i,j)*(1+tau_c);
               end
               if WB_c2(i,1) < C0*(1+G)^(j-1)+ C1*ICM2(i,j)
                  CSM0(i,j) = WB_c2(i,1);          
               else
                  CSM0(i,j) = C0*(1+G)^(j-1) + C1*ICM2(i,j);
               end
           end
           Tburden_diff = (sum(CSM(:,j),1)-sum(CSM0(:,j),1))/T_burden;          
       end
       % Now the real tau_c has been found!
       TaxRate(j,1) = tau_c;
       for i=1:npel    % Recompute real consumption tax burden given a new tau_c
           if WB_c2(i,1) < C0*(1+G)^(j-1) + C1*ICM2(i,j)*(1+tau_c)
              CSM(i,j) = WB_c2(i,1);       
           else
              CSM(i,j) = C0*(1+G)^(j-1) + C1*ICM2(i,j)*(1+tau_c);
           end
           if WB_c2(i,1) < C0*(1+G)^(j-1) + C1*ICM2(i,j)
              CSM0(i,j) = WB_c2(i,1);         
           else
              CSM0(i,j) = C0*(1+G)^(j-1) + C1*ICM2(i,j);
           end
       end
     % Transfer the rest consumption revenue to agents identically 
       Taxall_c = sum(CSM(:,j),1)-sum(CSM0(:,j),1);     % Total cpt revenue        
       y3 = zeros(npel,1);                              % 2nd time transfer
       if Taxall_c>num_sub3
          for i=1:npel
              y3(i,1) = (Taxall_c-num_sub3)/npel;
          end
       end
       ICM(:,j) = ICM2(:,j) + y2 + y3;          % Income after 2nd transfer       
       WE(:,j) = WB(:,j) + ICM(:,j) - CSM(:,j);         
    end
    WB(:,j+1) = WE(:,j);
end

end
