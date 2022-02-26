message("You MUST export the data this way to be used in the package")

# the directory of documentation for chapter 1
dir_docs01 <- file.path(dirname(getwd()), "FundamentalsCausalInference_docs",
                        "Brumback FOCI Website Material", "Chapter 1")
# the directory of documentation for chapter 3
dir_docs03 <- file.path(dirname(getwd()), "FundamentalsCausalInference_docs",
                        "Brumback FOCI Website Material", "Chapter 3")
# directory of data files
dir_data <- file.path(getwd(), "data")
# directory for functions
dir_lib <- file.path(getwd(), "lib")

dir_fciR <- 'C:\\Users\\Public\\MyR\\Packages\\fciR\\data'


load(file.path(dir_docs01, "whatifdat.RData"))
load(file.path(dir_docs01, "nces.RData"))
load(file.path(dir_docs01, "gss.RData"))
load(file.path(dir_docs03, "brfss.RData"))
load(file.path(dir_docs01, "cogdat.RData"))
load(file.path(dir_docs01, "doublewhatifdat.RData"))



# whatifdat <- data.frame(whatifdat)
# nces <- data.frame(nces)
# gss <- data.frame(gss)
# cogdat <- data.frame(cogdat)
# doublewhatifdat <- data.frame(doublewhatifdat)

save(whatifdat, file = file.path(dir_fciR, "whatifdat.rda"), version = 2)
save(nces, file = file.path(dir_fciR, "nces.rda"), version = 2)
save(gss, file = file.path(dir_fciR, "gss.rda"), version = 2)
save(cogdat, file = file.path(dir_fciR, "cogdat.rda"), version = 2)
save(doublewhatifdat, file = file.path(dir_fciR, "doublewhatifdat.rda"), version = 2)

# saveRDS(whatifdat, file = file.path(dir_fciR, "whatifdat.rds"))
# saveRDS(nces, file = file.path(dir_fciR, "nces.rds"))
# saveRDS(gss, file = file.path(dir_fciR, "gss.rds"))
# saveRDS(cogdat, file = file.path(dir_fciR, "cogdat.rds"))
# saveRDS(doublewhatifdat, file = file.path(dir_fciR, "doublewhatifdat.rds"))

