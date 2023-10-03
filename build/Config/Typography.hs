module Config.Typography where
import Utils (isUniqueList)

-- how many 1–n we want to generate <hr> versions of, to let us cycle ruler stylings instead of using a single ruler appearance everywhere.
cycleCount :: Int
cycleCount = 3

surnameFalsePositivesWhiteList :: [String]
surnameFalsePositivesWhiteList = isUniqueList ["Et", "et", "Al", "al", "Accurate", "Aesthetics", "Africa", "After", "Alert", "America", "An", "Apr",
                                  "April", "At", "Atari", "Atlas", "August", "Aug", "Autumn", "Before", "British", "Challenge", "Chat",
                                  "Codex", "Cohort", "Commodore", "Competition", "Considered", "Copyright", "Counterfactual", "Crypto",
                                  "Daily", "Dear", "Dec", "December", "Diaries", "Differences", "Early", "Enterprise", "Esthetics", "Evolution",
                                  "Expo", "Fair", "Fall", "Fanime", "Fanimecon", "Feb", "February", "First", "For", "Friday", "Impacts",
                                  "Jan", "January", "Jul", "July", "June", "Last", "Late", "Library", "Making", "Mar", "March", "May",
                                  "Memoirs", "Monday", "Monthly", "Ms", "Nov", "November", "Oct", "October", "Original", "Otakon", "Our",
                                  "Ours", "Over", "Predicting", "Reviews", "Sample", "Saturday", "Sci", "Security", "Sep", "September",
                                  "Since", "Spring", "Standard", "Statistics", "Suisse", "Summer", "Sunday", "Surface", "Survey",
                                  "Syntheses", "Than", "The", "Things", "Throughout", "Thursday", "Tuesday", "Until", "Wednesday", "Weekly",
                                  "Winter", "Writing", "Year", "Yearly", "Zilch", "In", "Judging", "From", "Experiment",
                                  "Between", "Caseness", "Goodbye", "By", "Around", "Christmas", "One", "Wars", "Michigan", "Act", "Circa",
                                  "Horizon", "Study", "Alamos", "Dover", "War", "London", "York", "Paris", "Berlin", "Francisco", "As", "Why",
                                  "Indicators", "China", "China’s", "Books", "During", "Halloween"]
