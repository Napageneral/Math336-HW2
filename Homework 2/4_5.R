principal = 219900
annual_rate = 0.05
monthly_rate = annual_rate/12

monthly_withdrawal = 1000
yearly_withdrawal=monthly_withdrawal*12

(principal-(yearly_withdrawal))*annual_rate

month_count=0
while (principal > 0) {
  principal = principal - monthly_withdrawal
  if(month_count%%12==0){
    principal=principal*(1+annual_rate)
  }
  month_count=month_count+1
}

month_count
month_count/12

