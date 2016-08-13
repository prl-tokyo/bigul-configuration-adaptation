import JSON
using Gadfly

adaptLog = JSON.parsefile("adapt3.log")
notAdaptLog = JSON.parsefile("notAdapt3.log")

p = plot(layer(x = map(x -> x["id"], adaptLog),
               y = map(x -> mean(x["results"]), adaptLog),
               Geom.point,
               Theme(highlight_width = 0pt)),

         layer(x = map(x -> x["id"], notAdaptLog),
               y = map(x -> mean(x["results"]), notAdaptLog),
               Geom.point, 
               Theme(default_color=colorant"red", highlight_width = 0pt)),

         layer(x = map(x -> x["id"], adaptLog),
               y = map(x->length(x["results"]), adaptLog),
               Geom.bar,
               Theme(default_color=colorant"gray", bar_spacing=0.5mm)),

         Scale.x_continuous(minvalue=0, maxvalue=70),

         Scale.y_log10,

         Guide.xlabel("Time (s)"),

         Guide.ylabel("Response time (ms)"),

         Guide.manual_color_key("Legend",
                                ["Number of Requests", "Self-adaptive", "Non-self-adaptive"],
                                ["gray", "deepskyblue", "red"]))

draw(PDF("myplot.pdf", 10inch, 5inch), p)
