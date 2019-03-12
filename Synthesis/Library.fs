module Synthesis

open System

let abelar number = number%12=0 && 12<number && number<3097

let area b h = match b>=0.0 && h>=0.0 with
    | true -> (b/2.0)*h
    | _ -> failwith "There's a negative value!"

let zollo number = match number<0 with
    | true -> number*(-1)
    | _ -> number*2

let min (a: int) (b: int) = match a>=b with
    | true -> b
    | _ -> a

let max a b = match a>b with
    | true -> a
    | _ -> b

let ofTime h m s = (h*60*60)+(m*60)+s

let toTime s =
    match s>0 with
    | true -> 
        let min = (s/60)%60
        let hours = (s/60)/60
        let remainSec = s%60
        hours,min,remainSec
    | _ -> 0,0,0

let digits number =
    let rec digNum num count= 
        match number/num=0 with
        | true -> count
        | false -> digNum (num*10) (count+1)
    digNum 10 1

let minmax values =
    let one,two,three,four = values
    min (min one two) (min three four), max (max (min one two) (max one four)) (max two three)

let isLeap year =
    match year%4=0,year%400=0,year%100=0,year<1582 with
    | _,_,_,true -> failwith "Input is less than 1582"
    | true,false,true,_ -> false
    | true,true,true,_ | true,_,_,_ -> true
    | _ -> false


let month number = match number>0 && number<=12,number with
    | true,1 -> "January",31
    | true,2 -> "February",28
    | true,3 -> "March",31
    | true,4 -> "April",30
    | true,5 -> "May",31
    | true,6 -> "June",30
    | true,7 -> "July",31
    | true,8 -> "August",31
    | true,9 -> "September",30
    | true,10 -> "October",31
    | true,11 -> "November",30
    | true,12 -> "December",31
    | _ -> failwith "Input is greater than 12"

let toBinary num = match num<0,num=0 with
    | true,false -> failwith ""
    | false,true -> "0"
    | _ ->  let rec solveBinary theValue binValue = match theValue=0 with
                | true -> binValue
                | _ -> 
                    let n = match theValue%2=0 with 
                        | true -> "0"  
                        | false -> "1"
                    solveBinary (theValue/2) (n + binValue )
            solveBinary num ""

let bizFuzz number = match number>2 with
    | true -> 
        let rec counter numCount divNum acc = match numCount <= number with
            | false -> acc
            | true ->
                let n = match numCount%divNum=0 with
                    | true -> acc+1
                    | false -> acc
                counter (numCount+1) divNum n
        ((counter 3 3 0), (counter 3 5 0), ((counter 3 3 0)+(counter 3 5 0))/8)
    | _ -> 0,0,0

let monthDay day year =
    let IssaLeap,range =
        match isLeap year with
        | true -> true,366
        | false -> false,365
    match day<=0, day>range with
    | false, true | true, false -> failwith "Error"
    | _ ->  let rec GetTheMonth daysLeft monthNum =
                let thisMonth,numDaysin = month monthNum
                let amendDays = 
                    match (thisMonth="February" && IssaLeap=true) with
                    | true -> numDaysin+1
                    | false -> numDaysin
                match daysLeft > amendDays with
                | false -> thisMonth
                | true -> GetTheMonth (daysLeft-amendDays) (monthNum+1)
            GetTheMonth day 1
 
let sqr n =
    let rec calculate guess i =
       match i with 
       | 10 -> guess
       | _ -> 
            let g = (guess + n/guess)/2.0
            calculate g (i+1)
    match n <= 0.0 with 
    | true -> failwith "Failed"
    | _ -> calculate (n/2.0) 0

let coord input =
    let co1, co2 = input
    let dist = fun (x,y) -> sqr (((co1-x)**2.0)+((co2-y)**2.0))
    let within = fun (x,y) width height -> (co1>=x && co1<=(x+width))&&(co2<=y && co2>=(y-height))
    dist,within
