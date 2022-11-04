#### code_stopword_creation.R                          ####
#### Creates custom stopword list for future analysis  ####

state_names = rio::import(here("data","state_names.csv"))
state_caps = rio::import("https://people.sc.fsu.edu/~jburkardt/datasets/states/state_capitals_name.txt", header = F) |> 
  mutate(cap = tolower(V2)) |> pull(cap)
customStop = c("alabama","alaska","arizona","arkansas","california",
               "colorado","connecticut","delaware","florida","georgia",
               "hawaii","hawai","hawai'i","idaho","illinois","indiana","iowa","kansas",
               "kentucky","louisiana","maine","maryland","massachusetts",
               "michigan","minnesota","mississippi","missouri","montana",
               "nebraska","nevada","new","hampshire","jersey","mexico",
               "york","north","carolina","dakota","ohio","oklahoma","oregon",
               "pennsylvania","rhode","island","tennessee","texas","utah",
               "vermont","virginia","washington","wisconsin","wyoming",
               "house","district","representative","senate","senator","senatorial",
               "governor","gubernatorial","governorship","question","crosstalk",
               "applause","laughter","moderator","candidate","gentlemen","ladies",
               "congressman","state","will","alaskan","thank","year","rebuttal",
               "speaker","lieutenant","upstate", "downstate", "make", "must",
               "assemblyman", "assemblywoman", "assemblyperson",  "want", "need", "inaudible",
               "commonwealth","aloha","isn","ve","didn","don",
               as.character(c(1998:2020)),
               state_names$name, # State resident names
               paste0(state_names$name,"s"),
               state_caps) # Plural
rio::export(customStop, here("data","customStop.rds"))