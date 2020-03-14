

net from http://www.stata-press.com/data/itsus/ 
* 胡乱改，写几个字 

*mkdir itsus_do
* 不是不是不是

net get itsus_files 

help itsus_files

*******
***ARCH model
***http:http://people.brandeis.edu/~pmherb/FIN250/_downloads/volatilityModels.pdf
use http://people.brandeis.edu/~pmherb/FIN250/_downloads/sp500.dta

cd "C:\Users\weifeng\Desktop\arch_model"

use sp500.dta,clear

* clear missing values 

drop if SP500 == . 

gen t = _n 

*时间序列设定

tsset t 

*画图
twoway line SP500  daten

twoway connected SP500  t

tsline SP500  //绘制图形

* geometric (log) returns 
gen lsp = log(SP500) 

gen lret = D.lsp   //求差值

*remove weak short range correlations with AR(2) 
gen dayofweek = dow(daten)   
//取星期几，0 = Sunday, 1 = Monday, ..., 6 = Saturday

gen datem = mofd(daten) 
//months since 1960m1

reg lret L1.lret L2.lret 

predict res, residual

*****Plot returns 
twoway line lret  daten

histogram lret,normal 

****
gen r2 = res*res 
ac r2  
//上面两张图包括了95%置信区间的阴影区域标注，在这一区间之外的那些相关都是个体显著的
//阴影部分是95%的置信区间，点落在95%置信区间之外或者附近，表明显著不为0，也就是有自相关

pac r2, srv

 estat archlm, lags(1/10)  //检验是否存在序列相关
 //Check autocorrelations of squared returns
 // test for ARCH effects in the residuals

 wntestq res, lags(10) 
 //the portmanteau (or Q) test for white noise


 //model 1

 arch res, garch(1) arch(1) 

 estat ic
 * get conditional variance forecasts for res 
 predict vhat, variance 
 * standardized residuals 
 gen sret = res/sqrt(vhat) 

 tsline sret


 //model 2
 arch res, garch(1/3) arch(1/3)

//model 3
*arch res, ar(1/P) ma(1/Q) garch(1/p) arch(1/q) 

arch lret, ar(1/2) garch(1) arch(1)

arch res, arch(1) garch(1) 

/*predict vhat, variance 
gen r2_new =res^2


*short macro to find mse, mae
capture program drop mse
program mse
	tempvar target
	gen `target' = `1' if `2' != .
	tempvar fres
	gen `fres' = `target' - `2'
	tempvar mse
	egen `mse' = mean(`fres'^2)
	tempvar mae
	egen `mae' = mean(abs(`fres'))
	tempvar v2
	egen `v2' = sd(`target')
	display "mse="`mse' " mse/v=" `mse'/`v2'^2
end
mse r2_new vhat
*/


** Crisis Dynamic Volatility Forecast
**Multi-period volatility forecast 

list t if daten==td(30sept2008) 
//13028

* 13028 time index for date
 tsset t 
 quietly arch res, arch(1) garch(1) 
 gen r2 = res^2 
 predict vhat, variance  dynamic(13028) 
 tsset daten, daily 
 tsline r2 vhat if daten>td(01jan2008), tlabel(,grid)


 **** Threshold ARCH
 
 *reg lret L1.lret L2.lret 
 *predict res, residual 
 gen r2 = res^2 
 * exponential filter variance fcast 
 tssmooth exp vhat1 = r2    //加权

 * GARCH(1,1) variance fcast 
 arch res, arch(1) garch(1) nolog 

 predict vhat2, variance 
 * GARCH(1,1) + TARCH(1) variance fcast 
 arch res, arch(1) garch(1) tarch(1) nolog 
 predict vhat3, variance 
 * compare mse do mse 
 mse r2 vhat1 
 mse r2 vhat2 
 mse r2 vhat3


 *****GARCH in the Mean 
 cd "C:\Users\weifeng\Desktop\arch_model"

 use gdpspread12 , clear

 lab var lgdp " log var of GDPC1"

 * calculate excess holding yield, y, on * 10-year vs 3-month 
 gen y = ((1+GS10/100)^2/(1+TB3MS[_n+1]/100)) /// 
        -(1+TB3MS/100) 

 * estimate garch-m; * risk premium is function of std. 
  arch y, garch(1) arch(1) archm archmexp(sqrt(X)) nolog 







