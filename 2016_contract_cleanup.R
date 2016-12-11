###
### Load
###
df <- read.csv('2016_All_Contracts_Full_20161015.csv')
nrow(df) # 3809946

###
### Anything missing? 
###
df <- df[!is.na(df$vendorname),]
nrow(df) # 3809946

df <- df[df$vendorname != 'NA',]
nrow(df) # 3809946

###
### Just to make things cleaner
###
vdf <- data.frame(df$vendorname, 
                  df$vendoralternatename, 
                  df$vendorlegalorganizationname,
                  df$phoneno,
                  df$streetaddress,
                  df$city,
                  df$state,
                  df$dunsnumber)
rm(df)
vdf_backup <- vdf


###
### Starting data set
### 
length(unique(vdf$df.vendorname)) # 141068
length(unique(vdf$df.dunsnumber)) # 136238

### I would expect the true number of distinct vendors to be 
### below 136238. Let's see if we can get close with name 
### matching. 


###
### I began my descent into the seven levels of regex hell. 
### This function employs regex to clean up a given column. 
###
### This would be much cleaner in Python with dictionaries --
### the key-value pairs would neatly replace the two vectors I
### constructed for iteration. 
###
strip <- function (column, verbose = TRUE) {
  
  expr_to_remove <- c("(^\\s*|\\s*$)",
                      "(,|\\.*|'|\\(|\\))",
                      "(\\slimited\\s?liability\\s?corp(oration)?)",
                      "(company$|corporation$|distribution$|services?$)",
                      "(,?(construction$|products$|corp(orate)?$))",
                      "(^the\\s|,?\\s?the$|,?\\s?inc(orporated)?$|,?\\s?llc$)",
                      "systems|(information\\stechnology)",
                      "\\s*$",
                      "&"
  )
  
  expr_to_replace <- c("",
                       "",
                       "",
                       "",
                       "",
                       "",
                       "",
                       "",
                       "and")
  
  u_column <- tolower(column)
  
  o_length <- length(unique(u_column))
  
  for (i in 1:length(expr_to_remove)) {
    
    if (verbose) {
      print(paste("Unique names:",length(unique(u_column))))
      print(paste("There are ",
                  length(grep(expr_to_remove[i],u_column)),
                  " matches to expression '",
                  expr_to_remove[i],
                  "'",
                  sep=""))
      u_column <- gsub(expr_to_remove[i],expr_to_replace[i],u_column)
      print("Removing...")
      new_length <- length(unique(u_column))
      print(paste("Unique names: ", 
                  new_length, 
                  " -- ", 
                  round((1-new_length/o_length)*100,2), 
                  "% reduction", 
                  sep=""))
    } else {
      u_column <- gsub(expr_to_remove[i],"",u_column)
    }
  }
  
  return(u_column)
}



### 
### Evaluate how well this should perform, using repeated
### draws of makeshift "test" sets. 
### Set a seed for reproducibility, then generate a sample,
### then determine the reduction in the dataset. 
### 

v = 0
tries = 100

for (i in seq(1,tries)) {
  set.seed(i)
  truth <- sample(vdf$df.vendorname,100)
  
  clean_truth <- strip(truth, verbose=F)
  #print(length(unique(clean_truth)))
  
  v = v + length(unique(clean_truth))
}

v = v/tries

print(paste("Expecting ",
            100-v,
            "% reduction in dataset just from name cleaning",
            sep=""))

# About 20% reduction expected 


###
### Clean up the other columns.
###
vdf$df.vendorname <- strip(vdf$df.vendorname)
vdf$df.vendoralternatename <- strip(vdf$df.vendoralternatename)
vdf$df.vendorlegalorganizationname <- strip(vdf$df.vendorlegalorganizationname)
vdf$df.phoneno <- strip(vdf$df.phoneno)
vdf$df.streetaddress <-strip(vdf$df.streetaddress)
vdf$df.city <- strip(vdf$df.city)

###
### For each row, compare 2 columns and coalesce the shorter 
### (non-zero) one to the target column. 
### Primarily intended to standardize name columns to the shortest one. 
###
coalesce <- function(df, c1, c2, target = 1) {
  col1 <- as.character(df[,c1])
  #print(col1)
  col2 <- as.character(df[,c2])
  #print(col2)
  
  for (i in 1:length(col1)) {
    if ((target == 1) & (nchar(col1[i]) > nchar(col2[i])) & (nchar(col2[i]) > 0)) {
      col1[i] <- col2[i]
    } else if ((target == 2) & (nchar(col2[i]) > nchar(col1[i])) & (nchar(col1[i]) > 0)) {
      col2[i] <- col1[i]
    }
  }
  
  df[,c1] <- col1
  df[,c2] <- col2
  
  return(df)
}

#test <- head(vdf)

vdf <- coalesce(vdf,1,2)
vdf <- coalesce(vdf,1,3)

length(unique(vdf$df.vendorname)) #133979
