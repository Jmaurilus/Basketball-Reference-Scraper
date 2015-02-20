getBREFTeamStatTable <- function(season_end = 2015, table_name = 'team', date = T){
  c('rvest','dplyr','pipeR') -> packages
  lapply(packages, library, character.only = T)
  'http://www.basketball-reference.com/leagues/' -> base
  'NBA' -> league
  '#' %>>% paste0(table_name) -> css_page
  css_page %>>% paste0(" , ", css_page,' a') -> css_id
  table_name %>>% tolower -> table_name
  table_name %>>% paste('stats', sep = "_") -> table
  base %>>% paste0(league,'_',season_end,".html") -> url
  url %>>% ## get table
    html %>>%
    html_nodes(css_page) %>>%
    html_table(header = F) %>>% data.frame() %>>% tbl_df() -> df
  
  if(df$X.1[1] == 'Rk'){
    df %>>%
      filter(X.1 == "Rk") %>>% as.character -> names
    'Rk' %>>% grep(x = df$X.1) -> row_of_header #find where rank is
    (row_of_header + 1) %>>% (df[.:nrow(df),]) -> df #skip that row and go to the end
    names %>>% tolower-> names(df)} else{
      df %>>%
        filter(X.1 == "Rk") %>>% as.character -> names
      'Rk' %>>% grep(x = df$X.1) -> row_of_header #find where rank is
      (row_of_header + 1) %>>% (df[.:nrow(df),]) -> df #skip that row and go to the end
      names %>>% tolower-> names(df)
    }
  names(df) %>>% (gsub('\\%|/','\\.',.)) -> names(df)
  NULL -> df$rk
  c('team','arena') -> table_name_character
  df[,!(df %>>% names) %in% table_name_character] %>>%
    apply(2, function(x) gsub('\\,','',x) %>>% as.numeric(x))  ->
    df[,!(df %>>% names) %in% table_name_character] #get rid of commas and make numeric
  df$team %>>% grepl(pattern = '\\*') -> df$playoff_team
  df$team %>>% (gsub('\\*','',.)) -> df$team
  df %>>% nrow() -1  -> rows
  df[1:rows,] -> df
  (season_end-1) %>>% paste0("-",season_end) -> season
  ##Grab Team Ids
  url %>>% ## get table
    html %>>%
    html_nodes(css_id) %>>%
    html_attrs() %>>% unlist %>>% as.character -> stems
  stems[3:length(stems)] -> stems #skip the 1st 2 rows since they are labels
  stems %>>% (gsub('\\/|.html|teams','',.)) %>>% #strip out the text
    (gsub(season_end,'',.)) -> bref_team_id #strip out the year to get the team id
  data.frame(season,table_name = table, bref_team_id, df) -> df  #combine into 1 df
  if(date == T){
    Sys.time() -> df$scrape_time #add scrape time if you want it
  }
  return(df)
}

#go from 2001, because earliest available date when shooting data available
#to go earlier, comment out parts about shooting
#only works from 1950 onwards for others, because league not NBA before

i = 2001
while (i < 2016)
{
  #getting extensions
  teamExtension = paste('team', i, '.csv', sep="", collapse = NULL)
  opponentExtension = paste('opponent', i, '.csv', sep="", collapse = NULL)
  miscExtension = paste('misc', i, '.csv', sep="", collapse = NULL)
  shootingExtension = paste('shooting', i, '.csv', sep="", collapse = NULL)
  shooting_oppExtension = paste('shooting_opp', i, '.csv', sep="", collapse = NULL)
  dir = '~/documents/projects/basketballData/'
  
  #full file extensions
  teamDir = paste(dir, teamExtension, sep="", collapse = NULL)
  opponentDir = paste(dir, opponentExtension, sep="", collapse = NULL)
  miscDir = paste(dir, miscExtension, sep="", collapse = NULL)
  
  #only from 2001 onwards
  shootingDir = paste(dir, shootingExtension, sep="", collapse = NULL)
  shooting_oppDir = paste(dir, shooting_oppExtension, sep="", collapse = NULL)
  
  #getting the tables
  getBREFTeamStatTable(season_end = i, table_name = 'team', date = F) -> team
  getBREFTeamStatTable(season_end = i, table_name = 'opponent', date = F) -> opponent
  getBREFTeamStatTable(season_end = i, table_name = 'misc', date = F) -> misc
  
  #only from 2001 onwards
  getBREFTeamStatTable(season_end = i, table_name = 'shooting', date = F) -> shooting
  getBREFTeamStatTable(season_end = i, table_name = 'shooting_opp', date = F) -> shooting_opp
  
  #writing to CSV
  team %>>% write.csv(teamDir, row.names = F)
  opponent %>>% write.csv(opponentDir, row.names = F)
  misc %>>% write.csv(miscDir, row.names = F)
  
  #only from 2001 onwards
  shooting %>>% write.csv(shootingDir, row.names = F)
  shooting_opp %>>% write.csv(shooting_oppDir, row.names = F)
  
  i <- i + 1
}