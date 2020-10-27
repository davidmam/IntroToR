# Code placed in this file fill be executed every time the
      # lesson is started. Any variables created here will show up in
      # the user's working directory and thus be accessible to them
      # throughout the lesson.
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

suppressMessages(dataset <- read_table2(file.path(.get_course_path(), "Intro_To_R", "Beginning_Plotting", "sport.txt")))
dataset$Sport <- as.factor(dataset$Sport)