
set.seed(NULL)

sl <- read.csv("~/Documents/Teaching/BIOL3641 - CIB/2023/Students.csv", header=T)

pt <- read.csv("Documents/Teaching/BIOL3641 - CIB/2023/CIB 2023-2024 _ Project allocation(1-78).csv", header=T)
pt <- pt %>% dplyr::select(c("Email", "Name", "Student.ID", "Favourite.project...1st.choice", "X2nd.choice", "X3rd.choice", "X4th.choice", "X5th.choice", "X6th.choice"))

nopref <- sl %>% filter(!Student.ID %in% pt$Student.ID) %>% mutate(Email=NA, Favourite.project...1st.choice=NA, X2nd.choice=NA, X3rd.choice=NA, X4th.choice=NA, X5th.choice=NA, X6th.choice=NA)
nopref <- nopref[,c(3,2,1,4,5,6,7,8,9)]
pt <- rbind(pt, nopref)

perm=round(nrow(pt)*0.01)
if(perm<1){perm<-1}

j=6
popALL <- c()
project.popularity <- for(i in 1:length(names(pt[,4:9]))){
  pop <- as.vector(unlist(rep(pt[,4:9][i],round(exp(j)))))
  popALL <- c(popALL, pop)
  j=j-1
}

projects <- sort(table(popALL))

#OPTIONAL - removing projects that are less popular
projects <- projects[-c(1,3,4)]

spl <- split(sample(pt$Student.ID), cut(seq(length(pt$Student.ID)), length(projects)))
spl <- spl[order(sapply(spl, length))]
names(spl) <- names(projects)

spl.score.total=0
for(i in pt$Student.ID){
  choice.i <- as.character(pt[pt$Student.ID==i,4:9])
  for (j in names(spl)){
    if(i %in% spl[[j]]){
      if(j %in% choice.i){
        score=exp(match(j,choice.i))
      } else{
        score=exp(7)
      }
      spl.score.total=spl.score.total+score
    }
  }
}

keepscores <- spl.score.total
bestscores <- spl.score.total
for(iteration in 1:30000){
  splB <- spl
  for(q in 1:perm){
    splC <- splB
    gc <- sample(names(splB),2)
    ss.1 <- sample(1:length(splB[[gc[[1]]]]),1)
    ss.2 <- sample(1:length(splB[[gc[[2]]]]),1)
    splB[[gc[[1]]]] <- c(splC[[gc[[1]]]][-ss.1],splC[[gc[[2]]]][ss.2])
    splB[[gc[[2]]]] <- c(splC[[gc[[2]]]][-ss.2],splC[[gc[[1]]]][ss.1])
    q=q+1
  }
  splB.score.total=0
  for(i in pt$Student.ID){
    choice.i <- as.character(pt[pt$Student.ID==i,4:9])
    for (j in names(splB)){
      if(i %in% splB[[j]]){
        if(j %in% choice.i){
          score=exp(match(j,choice.i))
        } else{
          score=exp(7)
        }
        splB.score.total=splB.score.total+score
      }
    }
  }
  keepscores <- c(keepscores, splB.score.total)
  if(splB.score.total<spl.score.total){
    spl <- splB
    spl.score.total <- splB.score.total
  }
  bestscores <- c(bestscores, spl.score.total)
  if(iteration%%100==0){
    print(paste("iteration ", iteration, "; ", length(unique(unlist(spl))), " students; score: ", round(spl.score.total), sep=""))
  }
  iteration=iteration+1
}

plot(keepscores, type='p', pch=1, cex=0.5)
plot(bestscores, type='l')

table.bystudent <- data.frame()
for(i in 1:length(spl)){
  for(j in 1:length(spl[[i]])){
    info.ij <- pt[pt$Student.ID==spl[[i]][[j]],] %>% dplyr::select(c("Email", "Name", "Student.ID"))
    choice.ij <- pt[pt$Student.ID==spl[[i]][[j]],] %>% dplyr::select(c("Favourite.project...1st.choice", "X2nd.choice", "X3rd.choice", "X4th.choice", "X5th.choice", "X6th.choice"))
    alloc.ij <- names(spl)[i]
    choice.allocated <- which(choice.ij %in% alloc.ij)
    if(length(choice.allocated)==0){choice.allocated<-"Not on the list"}
    all.ij <- as.vector(unlist(c(info.ij, c(gsub(" \\(.*\\)", "", names(spl)[i]), gsub("^.*\\(|\\)", "", names(spl)[i])), choice.allocated)))
    table.bystudent <- rbind(table.bystudent, all.ij)
  }
}
colnames(table.bystudent) <- c("Email", "Name", "Student.ID", "Project", "Supervisor", "#preference")
write.csv(table.bystudent, file="~/Documents/Teaching/BIOL3641 - CIB/2023/CIB_Groups_2023-2024.csv")






