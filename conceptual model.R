library(DiagrammeR)

mermaid("graph LR
        TRV(Transition from rice to vegetables)-->II(Increased income); linkStyle 0 stroke:blue, stroke-width:1.5px
        TRV(Transition from rice to vegetables)-->WU(Reduced water use); linkStyle 0 stroke:blue, stroke-width:1.5px
        TRV(Transition from rice to vegetables)-->IN(Improved nutrition); linkStyle 0 stroke:blue, stroke-width:1.5px
        TRV-->IC(Input costs); linkStyle 0 stroke:red, stroke-width:1.5px
        TRV-->LC(Labor costs); linkStyle 0 stroke:red, stroke-width:1.5px
        TRV-->MC(Machinery costs); linkStyle 0 stroke:red, stroke-width:1.5px
        TRV-->SF(Storage facilities); linkStyle 0 stroke:red, stroke-width:1.5px
        TRV-->TC(Transportation costs); linkStyle 0 stroke:red, stroke-width:1.5px
        II-->B(Total benefits);linkStyle 0 stroke:green, stroke-width:1.5px
        WU-->B;linkStyle 0 stroke:green, stroke-width:1.5px
        IN-->B;linkStyle 0 stroke:green, stroke-width:1.5px
        IC-->C(Total costs);linkStyle 0 stroke:red, stroke-width:1.5px
        LC-->C;linkStyle 0 stroke:red, stroke-width:1.5px
        MC-->C;linkStyle 0 stroke:red, stroke-width:1.5px
        SF-->C;linkStyle 0 stroke:red, stroke-width:1.5px
        TC-->C;linkStyle 0 stroke:red, stroke-width:1.5px
        B-->NPV(Net present value); linkStyle 0 stroke:green, stroke-width:1.5px
        C-->NPV;linkStyle 0 stroke:red, stroke-width:1.5px
        DR(Discount rate)-->NPV; linkStyle 0 stroke:black, stroke-width:1.5px")
