# Put custom tests in this file.
      
      # Uncommenting the following line of code will disable
      # auto-detection of new variables and thus prevent swirl from
      # executing every command twice, which can slow things down.
      
      # AUTO_DETECT_NEWVAR <- FALSE
      
      # However, this means that you should detect user-created
      # variables when appropriate. The answer test, creates_new_var()
      # can be used for for the purpose, but it also re-evaluates the
      # expression which the user entered, so care must be taken.
# Get the swirl state
getState <- function(){
   # Whenever swirl is running, its callback is at the top of its call stack.
   # Swirl's state, named e, is stored in the environment of the callback.
   environment(sys.function(1))$e
}

# Get the value which a user either entered directly or was computed
# by the command he or she entered.
getVal <- function(){
   getState()$val
}

# Get the last expression which the user entered at the R console.
getExpr <- function(){
   getState()$expr
}

# Get the names of variables created so far in the swirl lesson and
# which were saved in a protected environment for later use.
getVariableNames <- function(){
   names(getState()$snapshot)
}

# Get the saved value of a variable created earlier in a swirl lesson.
getVariable <- function(name_of_variable){
   stored <- getState()$snapshot
   name_of_variable <- as.character(name_of_variable)
   stored[[name_of_variable]]
}

# Remove a variable created earlier in a swirl lesson. This ability is useful
# for removing large variables which affect swirl's performance. It
# will not raise an error if the variable does not exist, but will return
# FALSE rather than TRUE.
removeVariable <- function(name_of_variable){
   snapshot <- getState()$snapshot
   if(!exists(name_of_variable, snapshot, inherit=FALSE))return(FALSE)
   survivors <- setdiff(names(snapshot), name_of_variable)
   getState()$snapshot <- snapshot[survivors]
}

# Stage a variable for inclusion is swirl's protected list. Staged variables
# will be included if and only if a user's answer passes all the current tests.
stageVariable <- function(name_of_variable, value){
   name_of_variable <- as.character(name_of_variable)
   e <- getState()
   e$delta[[name_of_variable]] <- value
}

# Get a copy of the list of staged variables.
getStagedVariables <- function(){
   getState()$delta
}

# Returns the current question (or text, figure, or whatnot) as a named list.
# This enables, for instance, access to the correct answer: currentQuestion()$CorrectAnswer.
getCurrentQuestion <-  function(){
   as.list(getState()$current.row)
}

# Returns the number of the current question.
getCurrentQuestionNumber <- function(){
   getState()$row
}

# Creates custom storage for course author's use. As a part of swirl's state, it will
# persist through swirl sessions and be saved to the hard drive aperiodically. An error 
# will be raised if the given name conflicts with another in swirl's state. Custom storage
# might be used to record performance statistics, for instance.
createCustomStore <- function(name_of_store){
   if(name_of_store %in% names(getState()))stop(paste(name_of_store, "is already in use. Use a different name."))
   assign(name_of_store, list(), getState())
}

# Creates a name:value pair in custom storage of the given name, creating the storage
# first if necessary.
store <- function(name_of_variable, value_of_variable, name_of_store){
   if(!exists(name_of_store, getState(), inherits=FALSE))createCustomStore(name_of_store)
   e <- getState()
   store <- get(name_of_store, e)
   store[[name_of_variable]] <- value_of_variable
   assign(name_of_store, store, e)
}

# Retrieves a value from custom storage, or returns NA if no such variable or store exists.
retrieve <- function(name_of_variable, name_of_store){
   e <- getState()
   if(!exists(name_of_store, e, inherits=FALSE))return(NA)
   store <- get(name_of_store, e, inherits=FALSE)
   if(!exists(name_of_variable, store))return(NA)
   store[[name_of_variable]]
}
check_plot_params = function( pars){
   x = getExpr()
   #x = 'plot(dataset$Weight, dataset$Height, xlab="Weight (kg)", ylab="Height (cm)")'
   found = TRUE
   for (p in pars) {
      found = found & ! is.na(grep(paste0('[, ]',p,' *= *[^, ]+'),x))
      
   }
   found == TRUE
}
plot_data <- function (x) {
   x = getExpr()
   found=TRUE
   found= found &! is.na(paste0(x[0],' *, *',x[1]),x)
   found == TRUE
}