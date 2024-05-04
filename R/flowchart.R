library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(magrittr)

exp_int <- boxGrob(glue("Expressed interest",
                        "n = {pop}", 
                           pop = txtInt(27),
                           .sep = "\n"))

excluded <- boxGrob(glue("Excluded (n = {tot}):",
                         " - not eligible: {ineligible}",
                         " - did not respond to interview request: {noint}",
                         tot = 30,
                         ineligible = 12,
                         noint = 30 - 12,
                         .sep = "\n"),
                    just = "left")

invited <- boxGrob(glue("Invited as a co-creator",
                        "n = {pop}", 
                        "Date = 17th January", 
                        pop = txtInt(12),
                        .sep = "\n"))

grid.newpage()
vert <- spreadVertical(exp_int, 
                       invited = invited, 
                       meet_greet = meet_greet, 
                       workshop_one = workshop_one, 
                       workshop_two = workshop_two)





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

team_baby_access <- boxGrob(glue("Use of TeamBaby web-app",
                             "n = {pop}", 
                             "Date = 7th February", 
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

workshop_proto <- boxGrob(glue("Workshop one",
                             "n = {pop}", 
                             "Date = 11th March", 
                             pop = txtInt(5),
                             .sep = "\n"))

workshop_methods <- boxGrob(glue("Workshop one",
                             "n = {pop}", 
                             "Date = 19th March", 
                             pop = txtInt(6),
                             .sep = "\n"))


                       



grid.newpage()
vert <- spreadVertical(org_cohort,
                       eligible = eligible,
                       included = included,
                       grps = grp_a)
grps <- alignVertical(reference = vert$grps,
                      grp_a, grp_b) %>%
  spreadHorizontal()
vert$grps <- NULL

excluded <- moveBox(excluded,
                    x = .8,
                    y = coords(vert$included)$top + distance(vert$eligible, vert$included, half = TRUE, center = FALSE))

for (i in 1:(length(vert) - 1)) {
  connectGrob(vert[[i]], vert[[i + 1]], type = "vert") %>%
    print
}
connectGrob(vert$included, grps[[1]], type = "N")
connectGrob(vert$included, grps[[2]], type = "N")

connectGrob(vert$eligible, excluded, type = "L")

# Print boxes
vert
grps
excluded