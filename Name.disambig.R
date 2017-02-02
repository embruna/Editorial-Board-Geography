Name.disambig <- function(A) {
  
  foo<-A
  foo2<-select(foo,author_id, FIRST_NAME,FirstInitialLast, FirstLast)
  foo3<-distinct(foo2) 
  foo4<-count(foo3,author_id=author_id) 
  foo4<-filter(foo4, n > 1)
  foo4$flag<-"DUPE_CHECK"
  foo5<-inner_join(foo, foo4, by = "author_id")
  foo6<-group_by(foo5,FirstLast)%>%filter(row_number()==1)
  
  return(foo6)
  
}
