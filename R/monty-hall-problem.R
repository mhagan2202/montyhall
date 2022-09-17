#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export

create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}

#' @title
#' Selecting a door
#' @description
#' `select_door()` is a function that will generate a door to be randomly
#' selected to which will then be identified as the users initial pick
#' @details
#' details regarding this function is that there are 3 doors to choose from
#' to which only one must be sampled from the 3 thus becoming "a.pick" and
#' being returned as such.
#' @param
#' the first argument is the vector of doors to which is numbered from 1-3
#' in this example.Therefore Identifying the amount of doors that are going to
#' to be used in the game. Following, the object a.pick must be assigned
#' based on the logical values that are being pulled from the variable 'doors'
#' it is first encapsulated by the function 'sample' which is ultimately use
#' in calling a certain 'sample' from the vector or 'x' that is specific. In
#' this case, that vector is 'doors' which is then followed by the argument
#' size of 1 which identifies the amount that is being 'sampled'. This logical
#' statement is assigned to the a.pick object which is then being returned from
#' the 'select_door' function.
#' @return
#' return is the door that is randomly assigned to the variable 'a.pick' based
#' on the logic as describe before which in this case is limited to doors 1-3.
#' when return function is being called it is essentially asking 'what should
#' be displayed when 'select_door' function is called?' How this value is
#' returned is based on the arguments which is stated prior
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}

#' @title
#' The reveal of the infamous goat door
#' @description
#' This function essentially opens one of the goat doors that the user
#' has not picked based on the 3 doors in total.This tactic is used to give
#' the user a choice to either stay with their current door or switch to the
#' other door giving the user a false perception that the odds are no 50/50.
#' @details
#' This piece is strictly written to open 1 of the 3 doors on the condition
#' that it cannot be one of the doors that the user picked and it can't
#' be one of the doors that has the car behind it.Therefore the code can
#' appear to be more complex than it actually is. The difficulty is assigning
#' doors to their proper homes within the code which would then allow the
#' program to effectively implement the conditions that have been stated prior.
#' @param
#'The first argument is the same from the 'select_doors' script which is
#'that the object doors holds a vector of numerical values from 1 to 3. The
#'next argument and the rest to come are based of if conditions. Which allows
#'for the system to run and tests to see if conditions are met based on the
#'arguments listed. In this case, if 'a.pick' that has been selected by the
#''select_doors' function happens to be a car, then the 'goat.doors' cannot be
#'the one with the car. The 'opened.door' object is then stating that from this
#'logic then the door that is allowed to be open are the 'goat.doors' and
#'the size of this 'sample' that we are select from is 1.Which this then allows
#'the program to select only 1 of the doors that has a goat in them to remove.
#'Same goes for the other if statement which basically states that if the
#'a.pick is a goat, then the 'opened.doors' cannot be the one with the car AND
#'the opened door cannot be the a.pick, since that would ruin the whole point
#'of the game. The reason this opened.door is a conditional statement is because
#'the choice is a little more clear for the program to pick the door that
#'has the goat behind it. If the user has already picked a goat door then there
#'is really only one other goat door for the program to pick. While for the
#'previous opened.door object needed to first identify which doors are
#'considered 'goat.doors' and then randomly sampling between the two of them
#'Since there are two doors to choose from you have to logically allow the
#'program to pick one of the two.
#' @return
#' The return is the opened.door object which has been thoroughly explained
#' on the conditions that surround the object based on the if condition of
#' a.pick.
#' @examples
#' open_goat_door()
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats
  if( game[ a.pick ] == "car" )
  {
    goat.doors <- doors[ game != "car" ]
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  {
    opened.door <- doors[ game != "car" & doors != a.pick ]
  }
  return( opened.door ) # number between 1 and 3
}

#' @title
#' The decision of a lifetime
#' @description
#' After the reveal of the goat door occurs the user is given a choice as to
#' weather they shall stay with their current selection for the door vs being
#' able to switch to the alternative door. This gives the false perception to
#' the user that they posess a 50/50 change of opening the door with the car
#' prize, When i reality this isn't entirely the case. After the user has
#' picked their door the goat door is then revealed which then only gives the
#' user two doors to choose from. The decision is then left to the user creating
#' a sense of uncertainty and tension within the audience.
#' @details
#' Details regarding the gameplay is simple and especially in the code. This
#' game is ulitmatly a pyschological game which plays on the inner conflict of
#' the individual, thus assuming that what isn't obtainable is often the answer
#' within our inner psyche. This montey hall problem was actually solved by
#' Marilyan vos Savant who was able to answer this question on a TV show which
#' qas about her being able to answer the most question, thus earning her title
#' as the "smartest person alive" She had recieved a lot of backlash for her
#' decision and rationale but ultimatley was proven right. This problem has
#' baffled mathematical professionals thus earning its reputation.
#' @param
#' Lets break down the arguements piece by piece, shall we? first arguement
#' again, is the doors function that has an assigned value of 1,2,3, which as
#' discussed from the previous question. The purpose for assining the numbers
#' 1,2,3 is to be associated with the criteria for the gameplay which has doors
#' 1 to 3. Now to the if statements, the first if statement is simply stating
#' that if user has decided to stay then the "final.pick" for the user shall
#' be the same as the initial pick, therefor no change is needed. On the
#' contrary, "! stay" if the user choose not to stay then the final pick
#' cannot be either the initial pick or cannot be the door that has already
#' been opened. Hence why this is a conditional statement for the final pick
#'  for the user that wishes not to stay.
#' @return
#' The return function is simply calling the statement final pick which has
#' been defined based on weather the user decides to stay or switch doors
#' one the user decides to stay or switch then the argument that is associated
#' with the final.pick , based on the decision of the user, is then executed.
#' This results will then populate one the change door function is called due
#' the return function being called at the end for 'final.pick'
#' @examples
#' change_door()
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }
  
  return( final.pick )  # number between 1 and 3
}

#' @title
#' It pays to win
#' @description
#' Details regarding this piece is specifically catered towards the individuals
#' that play this game. Of course after all the events take place for the
#' game play, there must be a winner at some point.
#' @details
#' Details regarding this piece is exactly the same as the description, but only
#' this code is specifically looking probabilities of user that are going to
#' win. Of course, the analysis of such intensity must be evacuate and
#' considered for the facts may not appear as they seem. It is obvious that the
#' winner determination is not directly solid as to weather switching or staying
#' would benefit the individual. But we can assess the likelihood that the
#' individuals is to win based on probability and statistics.Hence how this
#' program was born, by being able to numerically explain that probabilities of
#' success.
#' @param
#' Arguments related to the determine winner functions are as follows.
#' First addressing the if statements which have two primary results such
#' as 'Win' and 'Lose'. Within the Win function game is being called initially
#' since this game is being associated with this.game in particular and final.
#' pick that are being called from 'car' which the various combination have been
#' assigned from. Hence final.pick will either have the option of being a car
#' or not a car. which is what the second if statement indicates.
#' variable is the variable that is being called due to the ties that are being
#' @return
#' 'Win' or 'Lose'
#' @examples
#' determine_winner()
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}


#' @title
#' Let's play a game
#' @description
#' This function is specifically, calling to play the Monty python gameplay
#' that has been called and taking elements from the previous code.
#' @details
#' This piece of code is a collection of the prior codes that have been scripted
#' to assembly a coherent and playable game within the software. All compiled
#' into a single function to which initiates the game.
#' @param
#' Arguments within the function are as follows, new.game is being assigned to
#' create_game function, first pick. being assigned to the select_door and open.
#' door being the opened_goat_door on the condition that is is from new.game
#' and from the first.pick. Then there is the final pick stay and switch with
#' the only minor differences being the condition as to whether the user
#' decides to stay and of course calling the opened.door, first pick arguments
#' Since there are now two deriving point within the program we must follow
#' through on the pathways of both stay and switch in order to determine the
#' winner. hence both outcome stay and switch have the same arguments being
#' only minor difference being the the determine winner function would either
#' possess the final.pick.stay or final.pick switch argument and of course,
#' the last argument is new.game which is due to the determine_winner function
#' that is being called. In order for the determine winner to be selected
#' as the final outcome from the game is solution must be derived from the
#' initial question or creation of the particular problem that has been set in
#' place. Hence when determine_winner function is called first the
#' initial argument must be identified from either switch or stay to then
#' returning to the argument to which the determinate solution derives from
#' which in this case it, new.game. next we have the strategy variable which
#' is a simple vector between the switch and stay, outcome being derived from
#' the decision of their switching or staying and the results to that said
#' decision or outcome. and finally, the game results which combines the element
#' of both the strategy and outcome values in a nice data.frame.
#' @return
#' game.results which is the end results of both the strategy and outcome
#' values that has been mentioned in prior statement, in detail
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Do it again
#' @description
#' this export is specially looking at the repetition that allows the game to
#' be played as many times are the code requires
#' @details
#' The purpose for this piece is to look at the game play for a specified amount
#' of times that the user decides to input. The purpose for this is that it will
#' then allow us to simulate to the game as many times as we want in order to
#' get an accurate assessment on the best option for the game player to either
#' stay or switch.
#' @param
#' results.lists holds the value of lists which as been mentioned in the comment
#' is the collector of this information within the iterator. loop count minimum
#' has to be at least 1.  the for i is the amount specified within the how many
#' times the loop shall run and 1:n is the range for the data to which be
#' collected. game.outcome is from play_game which will simulate the game.
#' results list will then be saved in the game outcome. loop count will always
#' have to have a minimum of 2 for it to be conisdered technically a loop.
#' results.df is being saved from the results list and being bound from the
#' two data frames that are coming from each loop which will technically be it's
#'  own dateframe. Then it is all formated nice
#' to a cleaner format. then
#' @return
#' results.df which is being bound together from the various loop simulations
#' @examples
#' play_n_games(n=1000)
#' play_n_games(n=10)
#' play_n_games(n=50)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>%
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>%
    print()
  
  return( results.df )
  
}
