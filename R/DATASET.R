beamafilm <- dplyr::tibble(
  "Customer" = rep("Auckland Council", 12)
  , "Resource" = rep("Beamafilm", 12)
  , "Year" = rep("2022", 12)
  , "Month" = month.name
  , "Click" = round(runif(12, min = 1000, max = 20000))
  , "Renewal Date" = as.Date("2021-02-28")
)

linked_in_learning <- dplyr::tibble(
  "Start Day" = "2022-02-28"
  , "End Day" = "2022-03-31"
  , "People Logged In" = 1
  , "Unique Viewers" = 2
  , "Hours Viewed" = 3
  , "Course Views" = 4
  , "Course Completions" = 5
  , "Video Views" = 6
  , "Video Completions" = 7
  , "Learning Path Views" = 8
  , "Learning Path Completions" = 9
  , "Articles Views" = 10
  , "Article Completions" = 11
  , "Document Views" = 12
  , "Document Completions" = 13
)

# TODO figure out where to put this function
# writexl::write_excel(beamafilm, test_path("testdata", "beamafilm.xlsx"))
