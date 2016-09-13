import JSON
using Gadfly

adaptLog = JSON.parsefile("adapt3.log")
notAdaptLog = JSON.parsefile("notAdapt3.log")

p = plot(layer(x = map(x -> x["id"], adaptLog),
               y = map(x -> mean(x["results"]), adaptLog),
               xintercept = [10, 50],
               Geom.point,
               Geom.vline,
               Theme(highlight_width = 0pt)),

         layer(x = map(x -> x["id"], notAdaptLog),
               y = map(x -> mean(x["results"]), notAdaptLog),
               Geom.point, 
               Theme(default_color=colorant"red", highlight_width = 0pt)),

         Scale.x_continuous(minvalue=0, maxvalue=60),

         Scale.y_log10,

         Guide.xlabel("Time (s)"),

         Guide.ylabel("Response time (ms)"),

         Guide.manual_color_key("Legend",
                                ["Self-adaptive", "Non-self-adaptive"],
                                ["deepskyblue", "red"]))

p2 = plot(
         layer(x = map(x -> x["id"], adaptLog),
               y = map(x->length(x["results"]), adaptLog),
               Geom.bar,
               Theme(bar_spacing=0.5mm)),

         Guide.xlabel("Time (s)"),

         Guide.ylabel("Number of requests"),

         Scale.x_continuous(minvalue=0, maxvalue=70),

         Scale.y_continuous(minvalue=0, maxvalue=30)
         )

draw(PDF("myplot1.pdf", 10inch, 5inch), p)

draw(PDF("myplot2.pdf", 10inch, 5inch), p2)
