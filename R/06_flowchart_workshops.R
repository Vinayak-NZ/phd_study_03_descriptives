## ---- participant-flowchart

# part one of flochart diagram

exp_int <- boxGrob(glue("Expressed interest",
                        "n = {pop}", 
                        pop = txtInt(23),
                        .sep = "\n"))

invited <- boxGrob(glue("Invited as a co-creator",
                        "n = {pop}", 
                        "Date = 17th January", 
                        pop = txtInt(12),
                        .sep = "\n"))

meet_greet <- boxGrob(glue("Meet and greet",
                           "n = {pop}", 
                           "Date = 5th February", 
                           pop = txtInt(11),
                           .sep = "\n"))

workshop_one <- boxGrob(glue("Workshop one",
                             "n = {pop}", 
                             "Date = 6th February", 
                             pop = txtInt(11),
                             .sep = "\n"))

workshop_two <- boxGrob(glue("Workshop two",
                             "n = {pop}", 
                             "Date = 9th February", 
                             pop = txtInt(12),
                             .sep = "\n"))

excluded <- boxGrob(glue("Excluded (n = {tot}):",
                         " - not eligible: {ineligible}",
                         " - not selected based on interview: {noint}",
                         tot = 15,
                         ineligible = 3,
                         noint = 12,
                         .sep = "\n"),
                    just = "left")

png("output/flowchart_part_one.png", 
    width=500, 
    height = 500, 
    units = "px")

grid.newpage()

part_one <- spreadVertical(exp_int = exp_int,
                           invited = invited,
                           meet_greet = meet_greet,
                           workshop_one = workshop_one, 
                           workshop_two = workshop_two)

excluded <- moveBox(excluded,
                    x = .8,
                    y = coords(part_one$invited)$top + 
                      distance(part_one$exp_int, 
                               part_one$invited, 
                               half = TRUE, 
                               center = TRUE))

for (i in 1:(length(part_one) - 1)) {
  connectGrob(part_one[[i]], part_one[[i + 1]], type = "vert") %>%
    print
}

connectGrob(part_one$exp_int, excluded, type = "L")

part_one
excluded

dev.off()

# part two of flochart diagram

team_baby_access <- boxGrob(glue("DHI A access",
                                 "n = {pop}", 
                                 "Date = 10th February", 
                                 pop = txtInt(10),
                                 .sep = "\n"))

workshop_three <- boxGrob(glue("Workshop three",
                               "n = {pop}", 
                               "Date = 14th February", 
                               pop = txtInt(12),
                               .sep = "\n"))

workshop_four <- boxGrob(glue("Workshop four",
                              "n = {pop}", 
                              "Date = 20th February", 
                              pop = txtInt(11),
                              .sep = "\n"))

workshop_proto <- boxGrob(glue("Prototype workshop",
                               "n = {pop}", 
                               "Date = 11th March", 
                               pop = txtInt(5),
                               .sep = "\n"))

workshop_methods <- boxGrob(glue("Methods workshop",
                                 "n = {pop}", 
                                 "Date = 19th March", 
                                 pop = txtInt(6),
                                 .sep = "\n"))

png("output/flowchart_part_two.png", 
    width=500, 
    height = 500, 
    units = "px")

grid.newpage()

part_two <- spreadVertical(team_baby_access = team_baby_access,
                           workshop_three = workshop_three,
                           workshop_four = workshop_four, 
                           grps = workshop_proto)

grps <- alignVertical(reference = part_two$grps,
                      workshop_proto, workshop_methods) %>%
  spreadHorizontal()

part_two$grps <- NULL

for (i in 1:(length(part_two) - 1)) {
  connectGrob(part_two[[i]], part_two[[i + 1]], type = "vert") %>%
    print
}

connectGrob(part_two$workshop_four, grps[[1]], type = "N")
connectGrob(part_two$workshop_four, grps[[2]], type = "N")

part_two
grps

dev.off()
