if(!'devtools'%in%installed.packages()) tryCatch(install.packages('devtools'), error = function(e) warning(e$message, call. = FALSE))
splot.update=function(...) tryCatch(devtools::install_github('miserman/splot',...), error = function(e) warning(e$message, call. = FALSE))
lingmatch.update=function(...) tryCatch(devtools::install_github('miserman/lingmatch',...), error = function(e) warning(e$message, call. = FALSE))
if(!'splot'%in%installed.packages()) splot.update()
if(!'lingmatch'%in%installed.packages()) lingmatch.update()
require(splot, quietly=TRUE)
require(lingmatch, quietly=TRUE)

message('LUSI lab library loaded:
  partial documentation at www.depts.ttu.edu/psy/lusi/resources.php
  development versions of splot and lingmatch were also loaded;
  use splot.update() or lingmatch.update() to get the latest versions,
  and see ?splot or ?lingmatch for help.')

single_reviews=function(title,pages,write=TRUE,path=getwd(),filename){
  url=paste0('https://www.rottentomatoes.com/m/',title,'/reviews/?page=1&type=user')
  tt=tryCatch(readLines(url,warn=FALSE),error=function(e)NULL)
  if(is.null(tt)) return(tt)
  op=data.frame()
  np=max(1,as.integer(gsub('[^0-9]','',
    gsub('^.*Page \\d+ of |</span> <a class=\\"btn btn[^$]*','',tt[grep('pageInfo',tt,fixed=TRUE)])
  )))
  if((missing(pages) || is.null(pages)) || (is.numeric(pages) && pages>np)) pages=np
  cat('\n\npulling reviews for',title,'\n')
  for(p in seq_len(pages)){
    if(p!=1){
      url=paste0('https://www.rottentomatoes.com/m/',title,'/reviews/?page=',p,'&type=user')
      tt=tryCatch(readLines(url,warn=FALSE),error=function(e)NULL)
    }
    if(is.null(tt)) next
    cat('\rpage',p,'of',pages)
    revs=strsplit(tt[grep('review_table',tt,fixed=TRUE)],'<div class="row review_table_row">',fixed=TRUE)[[1]][-1]
    user=as.integer(sub('[^0-9]','',sub('\\/.*','',sub('^.*\\/user\\/id\\/','',revs))))
    super_reviewer=grepl('Super Reviewer',revs,fixed=TRUE)
    rating=unlist(lapply(strsplit(revs,'span',fixed=TRUE),function(r)length(grep('glyphicon-star',r,fixed=TRUE))))
    if(length(half<-grep('&frac12;',revs,fixed=TRUE))!=0) rating[half]=rating[half]+.5
    date=sub('<.*','',sub('^.*subtle\\">','',revs))
    text=sub('</div>.*','',sub('^.*</span></div> ','',revs))
    text=gsub('&#39;',"'",text,fixed=TRUE)
    text=gsub('&quot;','"',text,fixed=TRUE)
    text=gsub('<[^>]*>',' ',text)
    op=rbind(op,cbind(user,super_reviewer,user_profile=paste0('https://www.rottentomatoes.com/user/id/',user),
      rating,text,date,url=rep(url,length(user))))
  }
  if(nrow(op)!=0 && write || !missing(path) || !missing(filename)){
    if(missing(filename)) filename=paste0(title,'_reviews')
    path=paste0(sub('/+$','',path),'/',filename,'.csv')
    tryCatch(write.csv(op,path,row.names=FALSE),
      error=function(e)warning('failed to save file: ',e$message,call.=FALSE))
    cat('\nfile saved to',path)
  }
  invisible(op)
}

movie_reviews=function(titles,pages=NULL,write=TRUE,path=getwd(),filename){
  op=data.frame()
  for(t in titles){
    tt=tryCatch(single_reviews(t,pages,FALSE),error=function(e){
      warning('failed to pull reviews for ',t,': ',e$message,call.=FALSE)
      NULL
    })
    if(is.null(tt)) next
    tt$title=rep(t,nrow(tt))
    op=rbind(op,tt)
  }
  if(nrow(op)!=0 && write || !missing(path) || !missing(filename)){
    if(missing(filename)) filename=paste0(if(length(titles)==1) titles else 'movie','_reviews_',Sys.Date())
    path=paste0(sub('/+$','',path),'/',filename,'.csv')
    tryCatch(write.csv(op,path,row.names=FALSE),
      error=function(e)warning('failed to save file: ',e$message,call.=FALSE))
    cat('\n\nfile saved to',path)
  }
  invisible(op)
}

movie_ratings_links = function(titles, add = '', retry = TRUE, search_source = 'https://search.yahoo.com/search?q='){
  r = q = list()
  rt = NULL
  al = length(add)
  tl = length(titles)
  if(al < tl) add[seq(al + 1, tl)] = '' else if(al != tl) add = add[seq_len(tl)]
  names(add) = titles
  gs = function(title, add, sites = c('imdb', 'rottentomatoes', 'metacritic')){
    tryCatch({
      s = readLines(paste0(search_source, if(length(sites) == 1) paste0(if(!grepl('\\W', title)) 'movie', '+', sites) else
          'movie+reviews', if(add == '') paste0('+"', title, '"') else paste0('+', add, '+"', title, '"')), warn = FALSE)
      s = grep('https://www.', paste(s, collapse = ' '), fixed = TRUE, value = TRUE)
      s = unique(regmatches(s, gregexpr(paste0('https://www\\.(',
        paste(sites, collapse = '|'), ')\\.com/(title|m|movie)/[^ &?"%</]+'), s))[[1]])
      vapply(sites, function(o) grep(o, s, value = TRUE, fixed = TRUE)[1], '')
    }, error = function(e){
      if(grepl('503|999', e$message)) Sys.sleep(2)
      NA
    })
  }
  st = function(titles) for(i in seq_along(titles)){
    if(!i %% 20) Sys.sleep(2)
    title = sub('\\.(txt|rda)$', '', titles[i])
    res = gs(title, add[i])
    if(length(res) == 1 && is.na(res)){
      rt <<- unique(c(rt, title))
    }else if(any(is.na(res))){
      for(nr in seq_along(res)) if(is.na(res[[nr]])) q <<- c(q, list(c(title, names(res)[nr])))
    }
    r[[title]] <<- res
  }
  st(titles)
  if(retry){
    if(!is.null(rt)){
      Sys.sleep(5)
      titles = rt
      rt = c()
      st(titles)
    }
    if(length(q)) for(i in q) r[[i[1]]][i[2]] = gs(i[1], add[i[1]], i[2])
  }
  list(links = as.data.frame(do.call(rbind, r)), failed = rt, missings = q)
}

imdb_meta = function(urls){
  r = data.frame(
    url = character(),
    title = character(),
    original_title = character(),
    author = character(),
    director = character(),
    genre = character(),
    content_rating = character(),
    date = character(),
    hours = numeric(),
    rating = numeric(),
    nusers = numeric(),
    budget = numeric(),
    opening = numeric(),
    usgross = numeric()
  )
  for(i in seq_along(urls)) r[i, ] = tryCatch({
    p = readLines(urls[[i]], warn = FALSE)
    bo = grep('<h4 class="inline">Budget:</h4>', p, fixed = TRUE, value = TRUE)
    opening = grep('<h4 class="inline">Opening Weekend USA:</h4>', p, fixed = TRUE, value = TRUE)
    gross = grep('<h4 class="inline">Gross USA:</h4>', p, fixed = TRUE, value = TRUE)
    title = grep('<h1 class="">', p, fixed = TRUE, value = TRUE)
    p = jsonlite::fromJSON(paste0('{', paste(
      p[seq(grep('<script type="application/ld+json">{', p, fixed = TRUE) + 1,
        grep('^}</script>', p)[1] - 1)], collapse = ' '), '}'))
    duration = if(is.null(p$duration)) NA else strsplit(p$duration, '', TRUE)[[1]]
    dl = length(duration)
    data.frame(
      url = urls[[i]],
      title = if(length(title)) sub('^<.*?>', '', sub('&n.*$', '', title)) else NA,
      original_title = if(is.null(p$name)) NA else p$name,
      author = if(is.null(p$creator$name) || (is.null(p$creator) && 'Person' %in% p$creator$`@type`)) NA else
        paste(unique(p$creator[p$creator$`@type` == 'Person', 'name']), collapse = ', '),
      director = if(is.null(p$director)) NA else paste(unique(p$director$name), collapse = ', '),
      genre = paste(p$genre, collapse = ', '),
      content_rating = if(is.null(p$contentRating)) NA else p$contentRating,
      date = if(is.null(p$datePublished)) NA else p$datePublished,
      hours = if(dl == 1) NA else if(duration[dl] == 'M'){
        as.numeric(gsub('[^0-9]', '', paste(duration, collapse = ''))) / 60
      }else as.numeric(paste0(duration[3])) + if(length(duration) > 4)
        as.numeric(gsub('[^0-9]', '', paste(duration[5:6], collapse = ''))) / 60 else 0,
      rating = if(is.null(p$aggregateRating$ratingValue)) NA else as.numeric(p$aggregateRating$ratingValue),
      nusers = if(is.null(p$aggregateRating$ratingValue)) NA else as.numeric(p$aggregateRating$ratingCount),
      budget = if(length(bo)) gsub('^[^$]+|[$,]', '', bo) else NA,
      opening = if(length(opening)) gsub('^[^$]+|[$,]', '', opening) else NA,
      usgross = if(length(gross)) gsub('^[^$]+|, <.*$|[$,]', '', gross) else NA
    )
  }, error = function(e) rep(NA, ncol(r)))
  r
}

rottentomatoes_meta = function(urls){
  r = data.frame(
    url = character(),
    title = character(),
    author = character(),
    director = character(),
    critic_average = numeric(),
    critic_rating = numeric(),
    critic_n = numeric(),
    critic_fresh = numeric(),
    critic_rotten = numeric(),
    user_average = numeric(),
    user_n = numeric(),
    user_percliked = numeric(),
    content_rating = character(),
    genres = character(),
    date = character(),
    boxoffice = character(),
    runtime = character()
  )
  for(i in seq_along(urls)) r[i, ] = tryCatch({
    p = readLines(urls[[i]], warn = FALSE)
    st = grep('<script type="application/ld+json"', p, fixed = TRUE)
    st = jsonlite::fromJSON(gsub('^ *<script[^>]+>|</script>', '', p[c(st, st + 1)]))
    si = jsonlite::fromJSON(gsub('.*= |;$', '', grep('.scoreInfo =', p, fixed = TRUE, value = TRUE)))
    meta = grep('<ul class="content-meta info">', p, fixed = TRUE)
    bo = grep('<div class="meta-label subtle">Box Office:', p, fixed = TRUE)
    rt = grep('<div class="meta-label subtle">Runtime:', p, fixed = TRUE)
    ra = grep('<span class="subtle superPageFontColor">Average Rating:', p, fixed = TRUE)[1]
    data.frame(
      url = urls[[i]],
      title = if(is.null(st$name)) NA else st$name,
      author = if(is.null(st$author)) NA else paste(unique(st$author$name), collapse = ', '),
      director = if(is.null(st$director)) NA else paste(unique(st$director$name), collapse = ', '),
      critic_average_fresh = if(is.null(si$tomatometerAllCritics$score)) NA else si$tomatometerAllCritics$score,
      critic_average_rating = if(is.null(si$tomatometerAllCritics$avgScore)) NA else si$tomatometerAllCritics$avgScore,
      critic_n = if(is.null(si$tomatometerAllCritics$numberOfReviews)) NA else si$tomatometerAllCritics$numberOfReviews,
      critic_fresh = if(is.null(si$tomatometerAllCritics$freshCount)) NA else si$tomatometerAllCritics$freshCount,
      critic_rotten = if(is.null(si$tomatometerAllCritics$rottenCount)) NA else si$tomatometerAllCritics$rottenCount,
      user_average = if(is.null(si$audienceAll$averageRating)) NA else as.numeric(si$audienceAll$averageRating),
      user_n = if(is.null(si$audienceAll$ratingCount)) NA else si$audienceAll$ratingCount,
      user_percliked = if(is.null(si$audienceAll$score)) NA else as.numeric(si$audienceAll$score),
      content_rating = if(is.null(st$contentRating)) NA else st$contentRating,
      genres = if(is.null(st$genre)) NA else paste(st$genre, collapse = ', '),
      date = sub('^[^0-9]+', '', strsplit(grep('<time datetime=\"',
        p, fixed = TRUE, value = TRUE)[1], 'T', TRUE)[[1]][1]),
      boxoffice = if(!length(bo)) NA else gsub('<[^>]+>|[$, ]+', '', p[bo + 1]),
      runtime = if(length(rt)) sub('^ +', '', p[rt + 3]) else NA
    )
  }, error = function(e) c(url = urls[[i]], rep(NA, ncol(r) - 1)))
  r
}

guess_sex = function(names, retry = TRUE, search_source = 'https://search.yahoo.com/search?q='){
  op = data.frame(name = names, guess = NA, confidence = NA, female = NA, male = NA)
  female = " she | she's | her | hers | woman | (wife|sister|mother|gandmother|aunt) (of|to) "
  male = " he | he's | him | his | man | (husband|brother|father|grandfather|uncle) (of|to) "
  for(n in names) op[op$name == n, -1] = tryCatch({
    p = paste('', paste(gsub("[^A-z ']+", ' ',
      tolower(readLines(paste0(search_source, gsub('[ _&-]+', '+', n)), warn = FALSE))), collapse = ' '), '')
    fem = length(strsplit(p, female, perl = TRUE)[[1]])
    mal = length(strsplit(p, male, perl = TRUE)[[1]])
    conf = fem / (fem + mal)
    data.frame(if(fem > mal) 'female' else 'male', if(fem > mal) conf else 1 - conf, fem, mal)
  }, error = function(e){
    if(grepl('503|999', e$message)) Sys.sleep(5)
    rep(NA, ncol(op) - 1)
  })
  if(retry && any(su <- is.na(op$guess))){
    Sys.sleep(5)
    op[su,] = guess_sex(op[su, 'name'], FALSE)
  }
  op
}

reddit=function(topics,search=NULL,sort='hot',filename='reddit.csv',write=TRUE,lim=100,filter='\\[removed\\]',clean=TRUE,...){
  if(!'RedditExtractoR'%in%installed.packages()) install.packages('RedditExtractoR')
  if(!'jsonlite'%in%installed.packages()) install.packages('jsonlite')
  saf=options('stringsAsFactors')[[1]]
  options(stringsAsFactors=FALSE)
  on.exit(options(stringsAsFactors=saf))
  if(!missing(search)){
    cat('collecting urls from search...\n')
    tl=tryCatch(RedditExtractoR::reddit_urls(search_terms=search,subreddit=if(missing(topics))NA else topics[1]
      ,page_threshold=ifelse(lim<25,1,round(lim/25)),...)$URL,error=function(e)stop(e$message,call.=FALSE))
    topics=1
  }
  sop=c('hot','new','rising','top','gilded','ads')
  sort=sop[pmatch(tolower(sort),sop)]
  d=NULL
  for(t in topics){
    if(missing(search)){
      bl=paste0('https://www.reddit.com/r/',t,'/')
      tl=tryCatch(jsonlite::fromJSON(readLines(paste0(bl,'.json?limit=',lim),warn=FALSE)),
        error=function(e){warning('failed to get links from /r/',t);NULL})
      if(is.null(tl)) next else tl=paste0(bl,tl$data$children$data$id,'.json',if(sort!='hot')paste0('?sort=',sort))
    }
    cat(paste0('collecting comments from ',if(missing(search)) paste0('/r/',t) else paste('search:',search),'\n'))
    for(p in seq(tl)){
      cat(paste(p,'/',length(tl),':',gsub('\\.json.*$|\\?ref.*$','/',tl[p]),'\n'))
      d=rbind(d,RedditExtractoR::reddit_content(sub('\\.json.*','',tl[p])))
    }
  }
  d=d[!grepl(filter,d$comment),]
  if(clean) d$comment=gsub('[\034\035]','"',gsub('[\030\031]',"'",d$comment))
    if(write){
      message(paste0('output saved to ',getwd(),'/',filename))
      write.csv(d,filename,row.names=FALSE)
    }
  invisible(d)
}

reddit.karma=function(users){
  uu=unique(users)
  pu=function(u){
    p=tryCatch(readLines(paste0('https://old.reddit.com/user/',u),warn=FALSE),error=function(e)NULL)
    if(!is.null(p)){
      p=p[grep('karma',p)[1]]
      p=strsplit(p,if(any(grepl('class="karma"',p,fixed=TRUE)))'karma"'else' Karma',fixed=TRUE)[[1]]
      p=if(length(p)==4){
        p=p[1:3]
        vapply(regmatches(p,regexec('>[^>]*$',p)),function(s)as.numeric(gsub('[^0-9]','',s)),0)
      }else{
        p=p[2:3]
        p=as.numeric(gsub('[^0-9]','',sub('<.*$','',p)))
        c(p[1]+p[2],p)
      }
    }else c(NA,NA,NA)
  }
  if(length(uu)<length(users)){
    res=data.frame(User=users,Total=NA,Post=NA,Comment=NA)
    for(u in uu){
      su=users==u
      k=pu(u)
      rs=sum(su)
      res[su,-1]=if(rs==1) k else matrix(rep(k,rs),ncol=3,byrow=TRUE)
    }
  }else{
    res=data.frame(t(vapply(users,pu,c(0,0,0))))
    res=cbind(as.character(users),res)
    colnames(res)=c('User','Total','Post','Comment')
  }
  res
}

reddit.usercomments = reddit.userdata = function(users, filename, subreddits, lim = 100, type = 'comments'){
  if(!missing(type)) type = if(grepl('^c', type)) 'comments' else 'submitted'
  if(!missing(subreddits) && any(!grepl('r/',subreddits,fixed=TRUE))) subreddits=sub('^r/|^','r/',subreddits)
  d=do.call(rbind,lapply(unique(users),function(u) tryCatch({
    a = ''
    op = NULL
    while(lim > 0){
      tt = jsonlite::fromJSON(readLines(paste0('https://www.reddit.com/user/',
        u, '/', type, '/.json?limit=', lim, '&after=', a), warn = FALSE))
      nr = nrow(tt$data$children$data)
      if(nr){
        tt$data$children$data[, vapply(tt$data$children$data, class, '') %in% c('list', 'data.frame')] = NA
        op = rbind(op, tt$data$children$data)
        if(!is.null(tt$data$after)){
          a = tt$data$after
          lim = lim - nrow(tt$data$children$data)
        }else lim = 0
      }else lim = 0
    }
    op
  },error=function(e)NULL)))
  if(!missing(subreddits)) d=d[d$subreddit_name_prefixed%in%subreddits,]
  if(!missing(filename)){
    write.csv(d,filename,row.names=FALSE)
    message('file saved to ',paste0(getwd(),'/',filename))
  }
  d
}

convert_date=function(d) format(as.Date(as.POSIXct(d,origin='1970-01-01')),'%d-%m-%y')

reddit.lsm=function(data){
  data$post_text=do.call(paste,lapply(data[,c('title','post_text')],as.character))
  data$post_text=gsub('\031',"'",data$post_text,fixed=TRUE)
  data$title=gsub('\031',"'",as.character(data$title),fixed=TRUE)
  data$comment=gsub('\031',"'",data$comment,fixed=TRUE)
  ptxt=unique(data$title)
  posts=data.frame(lma_termcat(lma_weight(lma_dtm(ptxt))))
  if(ncol(posts)==1) posts=data.frame(t(posts))
  rownames(posts)=ptxt
  thread=as.data.frame(t(vapply(as.character(data$structure),function(id){
    s=strsplit(id,'_(?=\\d+$)',perl=TRUE)[[1]]
    if(length(s)==1) s=c(0,s)
    c(conv=s[1],index=s[2])
  },c('',''))),row.names=FALSE)
  thread$title=data$title
  thread$lsm=NA
  for(p in ptxt) tryCatch({
    su=thread$title==p
    d=data[su,]
    if(nrow(d)==1){
      thread[su,'lsm']=lingmatch(data[su,'comment'],data[su,'post_text'],type='lsm',drop=FALSE)$sim
      next
    }
    comments=lma_termcat(lma_weight(lma_dtm(d$comment)))
    comments=cbind(thread[thread$title==p,],comments)
    nc=ncol(comments)-4
    uc=as.character(unique(comments$conv))
    cs=t(vapply(uc[-1],function(r){
      tsu=if(grepl('_',r,fixed=TRUE)) with(comments,conv==(h<-strsplit(r,'_(?=\\d+$)',
        perl=TRUE)[[1]])[1] & index==h[2]) else with(comments,conv==0 & index==r)
      if(sum(tsu)==0) as.list(rep(NA,nc)) else comments[tsu,-(1:4)]
    },as.list(numeric(nc))))
    if(is.null(colnames(cs))) colnames(cs)=colnames(posts)
    comp=data.frame(conv=uc,title=p,rbind(posts[p,],cs))
    thread[su,'lsm']=if(all(comments$conv==0)){
      lingmatch(comments[,-(1:4)],comp,type='lsm',drop=FALSE)$sim[,1]
    }else lingmatch(comments,comp,group='conv',type='lsm',drop=FALSE)$sim[,2]
  },error=function(e)NULL)
  
  thread
}

alz.forums=function(source=c('discussion','archive')){
  sp=function(s){
    p=readLines(paste0('https://www.alzconnected.org/',s,'.aspx'),warn=FALSE)
    lapply(data.frame(rbind(
      grep('<span class="forumheading">',p,fixed=TRUE),
      grep('<td align="center" class="smallfont" nowrap="nowrap">',p,fixed=TRUE)
    )),function(r)c(p[seq(r[1],r[2])],s))
  }
  do.call(rbind,unname(lapply(do.call(base::c,lapply(source,sp)),function(f){
    fp=gsub('<[^>]*>','',f)
    data.frame(id=as.numeric(gsub('[^0-9]','',sub(fp[1],'',
      f[grep('<span class="forumheading">',f,fixed=TRUE)],fixed=TRUE))),source=fp[9],
      forum=fp[1],description=fp[3],topics=fp[6],posts=fp[7],last_post=sub('by .*','',fp[8]))
  })))
}

alz.topics=function(forums,topic_pages=1,sort=NULL,...){
  if(missing(forums)) forums=alz.forums(...)
  if(!is.data.frame(forums)){
    filter=forums
    forums=alz.forums()
    if(is.character(filter)) filter=unlist(lapply(filter,function(f)
      grep(f,as.character(forums$forum),TRUE,value=TRUE)),use.names=FALSE)
    forums=forums[forums[,if(is.numeric(filter))'id'else'forum']%in%filter,]
  }
  if(!'id'%in%colnames(forums)) stop('forums must contain a column of ids called id')
  if(!is.null(sort)) sort=paste0('&sortby=',if(grepl('date|time|can|last|post',sort,TRUE))
    '-' else match.arg(sort,c('title','starter','replies','views','-')))
  r=function(s,id,p) readLines(paste0('https://www.alzconnected.org/',
    s,'.aspx?g=topics&f=',id,'&page=',p,sort),warn=FALSE)
  op=as.list(rep(NA,nrow(forums)))
  for(i in seq_along(op)){
    f=r(forums$source[i],forums$id[i],1)
    p=grep('ekforumselectedpagelink',f,fixed=TRUE)
    if(length(p)!=0){
      p=strsplit(f[p],'page=',fixed=TRUE)[[1]]
      p=as.numeric(sub('".*','',p[length(p)]))
    }else{
      if(any(grepl('<td><img alt="" title=',f,fixed=TRUE))) p=1 else{
        warning('skipped ',forums$source[i],' forum ',forums$id[i],' (',forums$forum[i],')')
        next
      }
    }
    op[[i]]=do.call(rbind,lapply(seq_len(min(topic_pages,p)),function(p){
      if(p!=1) f=r(forums$source[i],forums$id[i],p)
      ts=lapply(data.frame(rbind(
        grep('<td><img alt="" title=',f,fixed=TRUE),
        grep('<td align="center" class="smallfont">',f,fixed=TRUE)
      )),function(r)f[seq(r[1],r[2])])
      do.call(rbind,unname(lapply(ts,function(to){
        tp=gsub('<[^>]*>','',to)
        data.frame(source=forums$source[i],forum=forums$forum[i],forum_id=forums$id[i],
          type=gsub("^.*title='|' .*",'',to[1]),id=as.numeric(gsub('^.*t=|\" class=.*','',to[2])),
          title=tp[2],author=tp[3],replies=as.numeric(tp[4]),views=as.numeric(tp[5]),
          last_post=sub('&nbsp;','',tp[6],fixed=TRUE))
      })))
    }))
  }
  do.call(rbind,op)
}

alz.posts=function(topics,su=replies!=0,post_pages=1,...,verbose=TRUE){
  if(missing(topics)) topics=alz.topics(...)
  if(!is.data.frame(topics)) topics=alz.topics(topics,...)
  su=substitute(su)
  if(!is.null(su)){
    su=eval(su,topics)
    su[is.na(su)] = FALSE
    if(sum(su)==0) stop('the current subset would remove all rows') else topics=topics[su,]
  }
  r=function(s,id,p) readLines(paste0('https://www.alzconnected.org/',
    s,'.aspx?g=posts&t=',id,'&page=',p),warn=FALSE)
  n=nrow(topics)
  op=as.list(rep(NA,n))
  for(i in seq_len(n)){
    if(verbose) cat('\rReading thread',i,'of',n)
    f=r(topics$source[i],topics$id[i],1)
    p=grep('ektopicselectedpagelink',f,fixed=TRUE)[1]
    if(!is.na(p)){
      p=strsplit(f[p],'page=',fixed=TRUE)[[1]]
      p=as.numeric(sub('".*','',p[length(p)]))
    }else{
      if(any(grepl('<tr class="postheader">',f,fixed=TRUE))) p=1 else{
        warning('skipped ',topics$source[i],' post ',topics$id[i],' (',topics$title[i],')')
        next
      }
    }
    op[[i]]=do.call(rbind,lapply(seq_len(min(post_pages,p)),function(p){
      if(p!=1) f=r(topics$source[i],topics$id[i],p)
      ts=lapply(data.frame(rbind(
        grep('<tr class="postheader">',f,fixed=TRUE)+3,
        grep('<tr class="postfooter">',f,fixed=TRUE)-3
      )),function(r)f[seq(r[1],r[2])])
      res=do.call(rbind,unname(lapply(ts,function(to){
        m=seq(grep('message ekMessage',to,fixed=TRUE)+1,length(to))
        if(topics$source[i]=='archive'){
          uma=sub('<.*','',to[m])
          to[1]=sub('^[^:]*: ','',uma[1])
          to[m]=sub(uma[1],'',to[m],fixed=TRUE)
        }
        tp=gsub('<[^>]*>|\t','',to)
        ui=strsplit(tp[m[1]-4],'Posts: ',fixed=TRUE)[[1]]
        d=strsplit(tp[9],', | ')[[1]]
        data.frame(source=topics$source[i],forum=topics$forum[i],forum_id=topics$forum_id[i],
          title=topics$title[i],id=topics$id[i],type=topics$type[i],replies=topics$replies[i],
          views=topics$views[i],user=tp[1],user_joined=sub('Joined: ','',ui[1],fixed=TRUE),
          user_posts=as.numeric(ui[2]),post=gsub('Ã‚','',paste(tp[m],collapse=' '),fixed=TRUE),
          dayofweek=d[1],month=d[2],day=as.numeric(d[3]),year=as.numeric(d[4]),
          time=if(grepl('PM',d[6])) paste({tt=as.numeric(strsplit(d[5],':')[[1]])
          if(tt[1]!=12)tt[1]=tt[1]+12;tt},collapse=':') else sub('^12','0',d[5]))
      })))
      cbind(index=seq_len(nrow(res)),res)
    }))
  }
  do.call(rbind,op)
}

scripts=function(genre,range=NULL,namesOnly=FALSE,dir=NULL,redownload=FALSE,pdf.dpi=400){
  ag=c('action','adventure','all','animation','comedy','crime','drama','family','fantasy','film-noir',
    'horror','musical','mystery','romance','sci-fi','short','thriller','war','western')
  if(length(genre) == 1 && !is.na(pmatch(tolower(genre), ag))){
    g = ag[pmatch(tolower(genre), ag)]
    names = tryCatch(readLines(
      if(g == 'all') 'https://www.imsdb.com/all%20scripts/' else paste0('https://www.imsdb.com/genre/',g),
      warn = FALSE
    ), error = function(e) stop(paste0("can't access scripts for ", g, ': ', e$message), call. = FALSE))
    names = strsplit(
      sub('^<br.*?p>', '', grep('<br><h1>', names, fixed = TRUE, value = TRUE)),
      '<br></p><p>', fixed = TRUE
    )[[1]]
    names = as.data.frame(do.call(rbind, lapply(names, function(n){
      n = strsplit(gsub('<.*?>', '~', n), '~+ *')[[1]][-1]
      c(n[1], gsub('^\\(| .*$', '', n[2]), gsub('^.*? |)$', '', n[2]), sub('Written by ', '', n[3]))
    })))
    colnames(names) = c('title', 'date', 'version', 'author')
    if(!missing(range)) names = names[range,]
    if(namesOnly) return(names)
    names = names$title
  }else{
    g = 'listed'
    names = genre
  }
  fn=if(is.null(dir)) paste(g,'scripts') else dir
  dir.create(fn, showWarnings=FALSE)
  message('saving ',g,' scripts:')
  ns=matrix(NA,1,1,dimnames=list(c(),'skipped scripts'))
  cfs=list.files(fn)
  ck=(!c('pdftools','tesseract')%in%installed.packages())
  wd=getwd()
  names = gsub(':', '', gsub(' ', '-', names, fixed = TRUE), fixed = TRUE)
  for(i in seq_along(names)){
    ext='.txt'
    if(!redownload && paste0(names[i],'.txt')%in%cfs) next
    script=tryCatch(suppressWarnings(readLines(paste0('https://www.imsdb.com/scripts/',
      names[i],'.html'), warn = FALSE)), error = function(e) '')
    if(length(script) > 1){
      s = grep('class="scrtext', script, fixed = TRUE)
      es = grep('<table', script, fixed = TRUE)
      script = gsub('<[^>]*>|\\\t','', paste(script[seq(s, es[es > s][1])], collapse = '\n'))
    }
    if(script=='' || (length(script) == 1 && nchar(script) < 500)){
      pf=paste0('https://www.imsdb.com/scripts/',names[i],'.pdf')
      if(ck[1]){
        install.packages('pdftools')
        ck[1]=FALSE
      }
      script = tryCatch(suppressMessages(pdftools::pdf_text(pf)), error = function(e) NULL)
      if(!is.null(script) && all(script=='')){
        cat('attempting to process ',names[i],'.pdf\n',sep='')
        if(ck[2]){
          install.packages('tesseract')
          ck[2]=FALSE
        }
        dir.create(td<-paste0(wd,'/',fn,'/',names[i]))
        setwd(td)
        on.exit({
          setwd(wd)
          unlink(td,TRUE)
        })
        script=tryCatch(tesseract::ocr(pdftools::pdf_convert(pf,dpi=pdf.dpi,verbose=FALSE))
          ,error=function(e)NULL)
        setwd(wd)
      }
      if(is.null(script)){
        ns=rbind(ns,names[i])
        next
      }
      script=paste(script,collapse='\n')
    }
    cat(i,'/',length(names),':',names[i],'\n')
    write(script,paste0(fn,'/',names[i],'.txt'))
  }
  message(paste0('scripts saved to ',wd,'/',fn,'/'))
  if(nrow(ns)>1) ns[-1,,drop=FALSE]
}

subtitles = function(ids, zip = 'subtitles_zip', out = 'subtitles', names = ids, rate_limit = 1,
  user_agent = 'TemporaryUserAgent', langid = 'eng', redownload = FALSE, unzip = TRUE, rename = TRUE,
  cleanup = TRUE){
  dir.create(zip, FALSE)
  dir.create(out, FALSE)
  if(!require('httr')){
    install.packages('httr')
    library('httr')
  }
  op = list(
    failures = c(),
    searches = list()
  )
  if(any(grepl('imdb\\.com|/', ids))){
    if(all(names == ids)) names = make.names(names, allow_ = TRUE)
    ids = gsub('[^0-9]', '', ids)
  }
  type = if(all(is.na(ids) | ids == 'NA' | grepl('^[0-9]{6,7}$', ids))) 'imdbid-' else{
    if(!require('stringdist')){
      install.packages('stringdist')
      library('stringdist')
    }
    ids = vapply(ids, URLencode, '')
    'query-'
  }
  for(i in seq_along(ids)) if(!is.na(ids[i]) && ids[i] != 'NA') tryCatch({
    z = paste0(zip, '/', names[i], '.zip')
    op$searches[[names[i]]] = GET(paste0('https://rest.opensubtitles.org/search/', type, ids[i]
                                         , '/sublanguageid-', langid), user_agent(user_agent))
    if(!has_content(op$searches[[names[i]]])){
      op$failures = c(op$failures, ids[i])
      warning('error in search: ', op$searches[[names[i]]]$status_code)
      return(invisible(op))
    }
    op$searches[[names[i]]] = content(op$searches[[names[i]]])
    op$searches[[names[i]]]$scores = vapply(op$searches[[names[i]]], function(r)
      if(all(c('IDMovieImdb', 'MovieName', 'SubRating', 'SubFromTrusted', 'SubLanguageID') %in% names(r))){
        (if(type == 'imdbid-') any(ids[i] == c(r$IDMovieImdb, paste0('0', r$IDMovieImdb))) else stringsim(names[i], r$MovieName)) *
          (as.numeric(r$SubRating) + .01) * (as.numeric(r$SubFromTrusted) + .5) * (r$SubLanguageID == langid)
      }else 0
      , 0)
    if(length(op$searches[[names[i]]]) == 1 || max(op$searches[[names[i]]]$scores) == 0) next
    op$searches[[names[i]]] = op$searches[[names[i]]][order(op$searches[[names[i]]]$scores, decreasing = TRUE)]
    l = op$searches[[names[i]]][[1]]$ZipDownloadLink
    if(is.null(l)){
      op$failures = c(op$failures, ids[i])
      next
    }
    dl = if(redownload || !file.exists(z)){
      GET(l, write_disk(z, TRUE))
      TRUE
    }else FALSE
    op$searches[[names[i]]]$file = unzip(z, list = TRUE)[1,1]
    if(unzip){
      unzip(z, op$searches[[names[i]]]$file, exdir = out)
      if(rename) file.rename(paste0(out, '/', op$searches[[names[i]]]$file), paste0(out, '/', names[i], '.srt'))
    }
    if(dl) Sys.sleep(abs(rnorm(1, rate_limit, rate_limit)))
  }, error = function(e){
    warning(e$message, call. = FALSE)
    op$failures <<- c(op$failures, ids[i])
  })
  if(unzip && cleanup){
    o = list.files(out)
    oi = if(rename) paste0(names, '.srt') else vapply(op$searches[names], '[[', '', 'file')
    names(oi) = paste0(names, '.zip')
    for(f in list.files(zip)) if(!is.na(oi[f]) && !oi[[f]] %in% o) file.remove(paste0(zip, '/', f))
  }
  invisible(op)
}

subtitles_clean = function(file, out = 'subtitles_clean', save = FALSE, overwrite = FALSE){
  name = gsub('^[^/]*/|\\.[^.]+$', '', file)
  o = paste0(out, '/', name, '.txt')
  if(overwrite || !file.exists(o)){
    s = readLines(file, warn = FALSE)
    s = s[-c(which(grepl('^[0-9]|^ *$', s) | s == ''), if(grepl('OpenSub', s[4], fixed = TRUE)) 3:4, seq_len(2) + length(s) - 2)]
    s = gsub('<[^>]*>', '', s)
    if(save){
      dir.create(out, FALSE)
      sink(o)
      cat(paste(s, collapse = '\n'))
      sink()
    }
  }else{
    s = readLines(o, warn = FALSE)
  }
  invisible(s)
}

split_texts=function(txt,group,segs=2,WC=NULL,word.break=' +'){
  if(missing(group)) group=seq_along(txt)
  txt=strsplit(txt,word.break)
  txt=lapply(seq_along(txt),function(i){
    l=length(txt[[i]])
    s=if(!is.null(WC)) WC else round(l/segs+.49)
    o=c()
    w=e=1
    while(w<l){e=min(w+s,l);o=c(o,paste(txt[[i]][w:e],collapse=' '));w=e+1}
    data.frame(group=group[i],segment=seq_along(o),text=o)
  })
  do.call(rbind,txt)
}

list2sm = function(l){
  cseq = function(x){
    x = sort(x)
    l = length(x)
    v = c = unique(x)
    i = 1
    for(u in seq_along(v)){
      n = i
      while(i < l && x[i] == x[i + 1]) i = i + 1
      c[u] = i - n + 1
      i = i + 1
    }
    data.frame(j = v, x = as.integer(c))
  }
  terms = unique(unlist(l, use.names = FALSE))
  do.call(sparseMatrix, c(do.call(rbind, lapply(seq_along(l), function(i){
    ms = cseq(match(l[[i]], terms))
    ms$i = rep(i, length(ms$j))
    ms
  })), list(dims = c(length(l), length(terms)), dimnames = list(NULL, terms))))
}

montyhall=function(doors=3,choice=sample(options,1),judge='random',nopen=1,iter=10000){
  stay=flip=switch=lbias=rbias=numeric(iter)
  options=seq(max(3,doors))
  nopen=min(nopen,doors-2)
  if(!missing(choice) && choice>doors) choice=doors
  cat('Doors:',length(options)
    ,'\nDoors opened:',nopen
    ,'\nInitial Choice:',ifelse(missing(choice),'random',choice)
    ,'\nJudge:',ifelse(grepl('ran',judge),'randomly chooses which available door to open',
      paste0('has a bias toward the ',ifelse(grepl('le',judge),'left','right'),'-most available door'))
    ,'\nIterations:',iter,'\n')
  for(i in 1:iter){
    car=sample(options,1)
    c1=eval(substitute(choice))
    jop=options[-c(car,c1)]
    open=if(length(jop)==1){ jop
    }else if(grepl('ran',judge)){ sample(jop,nopen)
    }else jop[ifelse(grepl('le',judge),(length(jop)-nopen+1):length(jop),1:nopen)]
    jop=options[-c1]
    aop=options[-c(open,c1)]
    stay[i]=c1==car
    switch[i]=ifelse(length(aop)==1,aop,sample(aop,1))==car
    flip[i]=sample(options[-open],1)==car
    lbias[i]=options[-open][length(options[-open])]==car
    rbias[i]=options[-open][1]==car
  }
  print(matrix(c(sum(stay),sum(switch),sum(flip),sum(lbias),sum(rbias))/iter
    ,ncol=1,dimnames=list(c('always stay','always switch','randomly choose after door is opened'
      ,'choose the left-most available door'
      ,'choose the right-most available door'),'win %')))
}

mclm=function(mod,n=200,sd=1,error.mean=0,int=0,s=10000){
  it=proc.time()[3]
  mod=eval(substitute(mod))
  l=length(mod)
  ns=names(mod)
  cat(paste0('\nSample size: ',n,'\nIterations: ',s,'\nModel: lm(',paste0('y~',paste(ns,collapse='+')),')\nProcessing... '))
  setb=as.numeric(sapply(mod,function(x)x[1]))
  bs=sds=se=t=p=data.frame(matrix(0,s,l+1,dimnames=list(c(),c('intercept',ns))))
  for(i in 1:s){
    idat=data.frame(matrix(0,n,l,dimnames=list(c(),ns)))
    for(v in 1:l){
      idat[,ns[v]]=if(is.numeric(mod[[v]])) mod[[v]][1:n+1] else{
        if(grepl('\\*|:',mod[[v]][2])) apply(idat[,strsplit(mod[[v]][2],'\\*|:')[[1]]],1,prod) else{
          do.call(mod[[v]][2],list(n,ifelse(is.na(mod[[v]][3]),0,as.numeric(mod[[v]][3])),ifelse(is.na(mod[[v]][4]),1,as.numeric(mod[[v]][5]))))
        }
      }
    }
    idat$y=int+as.matrix(idat)%*%setb+rnorm(n,error.mean,sd)
    m=summary(lm(paste0('y~',paste(ns,collapse='+')),data=idat))$coef
    bs[i,]=m[,1]
    sds[i,]=apply(idat,2,sd)
    se[i,]=m[,2]
    t[i,]=m[,3]
    p[i,]=m[,4]
  }
  res=matrix(c(int,setb,apply(bs,2,mean),apply(sds,2,mean),apply(se,2,mean),apply(t,2,mean),apply(p,2,mean),
    apply(p,2,function(x){sum(x<.05)/s})),l+1,7,dimnames=list(c('intercept',ns),c('set b','b','sd','se','t','p','power')))
  cat(paste('finished in',round(proc.time()[3]-it,3),'seconds\n\n'))
  print(res,digits=2)
  invisible(list(b=bs,sd=sds,se=se,t=t,p=p))
}

enet = function(x, y, ..., a = seq.int(0, 1, .25), cores = detectCores() - 2){
  if(!require('glmnet')){install.packages('glmnet'); library('glmnet')}
  if(!require('doParallel')){install.packages('doParallel'); library('doParallel')}
  ms = if(cores > 1){
    cl=makeCluster(cores)
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
    foreach(i = a, .packages = 'glmnet') %dopar%
      cv.glmnet(x, y, parallel = TRUE, alpha = i, ...)
  }else lapply(a, function(i) cv.glmnet(data.matrix(x), y, parallel = FALSE, alpha = i, ...))
  m = which.min(vapply(ms, function(a) a$cvm[a$lambda == a$lambda.1se], 0))
  list(alpha = a[m], model = ms[[m]])
}

lapplymc = function(sequence, fun, args = list(), ...,
  cores = max(1, parallel::detectCores() - 2)){
  if(!require('doParallel')){
    install.packages('doParallel')
    library('doParallel')
  }
  cl = makeCluster(cores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  foreach(i = sequence, ...) %dopar% do.call(fun, c(list(i), args))
}

fapply = function(x, pars = list(), xslot = 'x', funs = 'gsub'){
  fl = length(funs)
  pl = length(pars)
  xl = length(xslot)
  l = max(fl, pl, xl)
  if(fl < l) funs = rep_len(funs, l)
  if(pl < l) pars = if(pl) rep_len(pars, l)
  if(xl < l) xslot = rep_len(xslot, l)
  for(i in seq_len(l)){
    pars[[i]][[xslot[i]]] = x
    x = do.call(funs[[i]], pars[[i]])
  }
  x
}

repcv = function(model, reps = 1000, folds = 3, data, group = NULL,
  permute = TRUE, alpha = .05, cores = parallel::detectCores() - 2, packages = NULL){
  me = c(class(model) == 'lme', grepl('^lmer', class(model)))
  if(any(me) && missing(packages)) packages = if(me[1]) 'nlme' else 'lme4'
  if(missing(data)){
    data = if(me[2]) model@frame else if(me[1]) model$data else model$model
    if(is.null(data)) stop('data not found in model')
  }
  y = as.character(formula(model)[[2]])
  if(!missing(group)){
    group = with(data, group)
    if(length(group) != nrow(data) && is.numeric(group) && group <= ncol(data)) group = data[, group]
  }else if(me[2]) group = data[, names(model@flist)[1]] else if(me[1]) group = model$groups[,1]
  cn = if(me[1]) 'tTable' else 'coefficients'
  g = FALSE
  nf = seq_len(folds)
  if(is.null(group)) group = seq_len(nrow(data)) else g = anyDuplicated(group)
  ug = if(g) unique(group) else seq_len(nrow(data))
  ng = length(ug)
  exfun = if(me[2]) fixef else if(me[1]) function(m) m$coef$fixed else coef
  rf = function(r){
    s = split(ug, sample(nf, ng, TRUE))
    data$nully = sample(data[, y])
    sdat = lapply(s, function(su) data[if(g) group %in% su else su,])
    tryCatch({
      if(permute){
        res = lapply(sdat, function(d) list(
          real = exfun(update(model, data = d)),
          null = exfun(update(model, nully ~., data = d))
        ))
        abs(colMeans(do.call(rbind, lapply(res, '[[', 'null')))) >
        apply(do.call(rbind, lapply(res, '[[', 'real')), 2, function(b) min(abs(b)))
      }else colMeans(do.call(rbind, lapply(sdat, function(d)
        2 * (1 - pnorm(abs(summary(update(model, data = d))[[cn]][,3])))
      ))) > alpha
    }, error = function(e) NULL)
  }
  ps = do.call(rbind, if(cores > 1) lapplymc(seq_len(reps), rf, .packages = packages) else
    lapply(seq_len(reps), rf))
  res = summary(model)
  res$repcvs = nrow(ps)
  res[[cn]] = cbind(res[[cn]],'Pr(mean(p) > alpha)' = colMeans(ps))
  if(permute) colnames(res[[cn]])[ncol(res[[cn]])] = 'Pr(mean(null) > min(b))'
  res
}

bs=function(data,method='cor',alpha=.05,iter=10000){
  res=matrix(NA,iter,ncol(data)-1,dimnames=list(c(),colnames(data)[-1]))
  for(i in 1:iter){
    nd=data[sample(1:nrow(data),replace=TRUE),]
    for(c in 2:ncol(nd)) res[i,c-1]=switch(method,
      cor=cor(nd[,1],nd[,c]),
      lm=lm(nd[,1]~nd[,c])$coef[2]
    )
  }
  summary=matrix(NA,ncol(data)-1,4,dimnames=list(colnames(data)[-1],
    c(paste0((alpha/2)*100,'%'),'mean','sd',paste0((1-alpha/2)*100,'%'))))
  for(i in 1:nrow(summary)){
    qt=quantile(res[,i],c(alpha/2,1-alpha/2))
    summary[i,]=c(qt[1],mean(res[,i]),sd(res[,i]),qt[2])
  }
  print(summary,digits=2)
  list(res=res,summary=summary)
}

bspmed=bsmed=function(x,y,m,data=NULL,cov=NULL,random=NULL,su=NA,i=1000,method='permutation',alpha=.05,figure=TRUE,...){
  txt=list(
    x=gsub('^\\\\*"|\\\\*"$','',deparse(substitute(x))),
    y=gsub('^\\\\*"|\\\\*"$','',deparse(substitute(y))),
    m=gsub('^\\\\*"|\\\\*"$','',deparse(substitute(m))),
    random=gsub('^\\\\*"|\\\\*"$','',deparse(substitute(random)))
  )
  rand=!missing(random)
  covar=if(is.null(data) && !missing(cov)){
    warning('data must be specified to include covariates', call. = FALSE)
    FALSE
  }else !missing(cov)
  boot=grepl('^b',method,TRUE)
  if(!is.null(data)) data=as.data.frame(data)
  tdc=function(a,data=NULL){
    ta=a
    if(is.character(ta) && (length(ta)==1 || !any(grepl(' ',ta,fixed=TRUE)))) ta=parse(text=a)
    ta=tryCatch(eval(ta,data,parent.frame(2)),error=function(e)NULL)
    if(is.null(ta)) ta=tryCatch(eval(ta,data),error=function(e)NULL)
    if(is.null(ta)) stop('could not find ',a,call.=FALSE)
    ta
  }
  dat=data.frame(
    x=tdc(txt$x,data),
    y=tdc(txt$y,data),
    m=tdc(txt$m,data)
  )
  names(dat) = with(txt, c(x, y, m))
  if(rand) dat$random=tdc(txt$random,data)
  if(covar) dat = cbind(dat, data[, cov])
  for(c in seq_len(ncol(dat))) if(is.factor(dat[,c])) dat[,c]=as.numeric(dat[,c])
  mot=paste0('Effect of ',txt$x,' on ',txt$y,', by way of ',txt$m)
  cat(mot,'\n')
  if(rand) cat('Random:',paste0('~1|',txt$random),'\n')
  if(covar) cat('Covariates:',paste0(cov, collapse = ' + '),'\n')
  if(!missing(su)){
    cat('Subset:',deparse(substitute(su)),'\n')
    dat=if(!missing(data)) dat[eval(substitute(su),data),] else dat[su,]
  }
  dat=na.omit(dat)
  t=proc.time()[3]
  ss=seq_len(nrow(dat))
  amod = as.formula(with(txt, paste(m, '~', x, if(covar) paste(' +', paste(cov, collapse = ' + ')))))
  cmod = as.formula(with(txt, paste(y, '~', x, if(covar) paste(' +', paste(cov, collapse = ' + ')))))
  bmod = as.formula(with(txt, paste(y, '~', m, '+', x, if(covar) paste(' +', paste(cov, collapse = ' + ')))))
  if(!rand){
    a=lm(amod,dat)
    b=lm(bmod,dat)
    ab=if(boot){
      vapply(seq_len(i),function(p){
        nd=dat[sample(ss,replace=TRUE),]
        lm(amod,nd)$coef[2]*lm(bmod,nd)$coef[3]
      },0)
    }else{
      pr=lapply(list(a,b),function(m)list(f=fitted(m),r=resid(m)))
      vapply(seq_len(i),function(p){
        dat[, txt$m] = pr[[1]]$f + sample(pr[[1]]$r)
        dat[, txt$y] = pr[[2]]$f + sample(pr[[2]]$r)
        lm(amod,dat)$coef[2]*lm(bmod,dat)$coef[3]
      },0)
    }
    a=summary(a)$coef
    b=summary(b)$coef
  }else{
    if(!require('nlme')){
      install.packages('nlme')
      library('nlme')
    }
    gs=unique(dat$random)
    a=lme(amod,dat,~1|random,na.action=na.omit)
    b=lme(bmod,dat,~1|random,na.action=na.omit)
    ab=if(boot){
      vapply(seq_len(i),function(p){
        nd=dat[dat$random%in%sample(gs,replace=T),]
        a=tryCatch(summary(lme(amod,nd,~1|random,na.action=na.omit))$tTable[2,1],error=function(e)NULL)
        b=tryCatch(summary(lme(bmod,nd,~1|random,na.action=na.omit))$tTable[3,1],error=function(e)NULL)
        if(!is.null(a) && !is.null(b)) a*b else NA
      },0)
    }else{
      pr=lapply(list(a,b),function(m)list(f=fitted(m),r=resid(m)))
      vapply(seq_len(i),function(p){
        dat[, txt$m] = pr[[1]]$f + sample(pr[[1]]$r)
        dat[, txt$y] = pr[[2]]$f + sample(pr[[2]]$r)
        a=tryCatch(summary(lme(amod,dat,~1|random,na.action=na.omit))$tTable[2,1],error=function(e)NULL)
        b=tryCatch(summary(lme(bmod,dat,~1|random,na.action=na.omit))$tTable[3,1],error=function(e)NULL)
        if(!is.null(a) && !is.null(b)) a*b else NA
      },0)
    }
    a=summary(a)$tTable[,c(1,2,4,5)]
    b=summary(b)$tTable[,c(1,2,4,5)]
    if((fr<-mean(is.na(ab)))>.7) stop(round(fr*100,2),'% of the lme models failed')
  }
  cat('Processed in',proc.time()[3]-t,'seconds\n\n')
  message('Sobel')
  m=matrix(0,2,5,dimnames=list(c('b','p'),c('a','b','c',"c'",'indirect')))
  c=if(!rand) summary(lm(cmod,data=dat))$coef else summary(lme(cmod,data=dat,random=~1|random))$tTable[,c(1,2,4,5)]
  ie=a[2,1]*b[2,1]
  m[1,]=c(a[2,1],b[2,1],c[2,1],b[3,1],ie)
  m[2,]=c(a[2,4],b[2,4],c[2,4],b[3,4],(1-pnorm(abs(ie/sqrt(b[2,1]^2*a[2,2]^2+a[2]^2*b[2,2]^2))))*2)
  print(round(m,4))
  p=qnorm(sum(na.omit(ab)<=ie)/i)
  cat('\n')
  message(if(boot) 'Bootstrap confidence intervals' else 'Permutation confidence intervals')
  ci=quantile(ab,c(alpha/2,1-alpha/2),na.rm=TRUE)
  if(boot){
    res=cbind(res<-as.data.frame(matrix(c(ci,quantile(ab,c(pnorm(2*p+qnorm(alpha/2)),pnorm(2*p+qnorm(1-alpha/2)))
      ,na.rm=TRUE)),ncol=2,dimnames=list(c('Uncorrected','Corrected'),c(paste0((alpha/2)*100,'%'),
        paste0((1-alpha/2)*100,'%'))),byrow=T)),`all <|> 0`=apply(res,1,function(r) all(r<0) || all(r>0)))
  }else{
    res=cbind(as.data.frame(matrix(ci,1)),all(ci<ie) || all(ci>ie))
    rownames(res)='CI'
    colnames(res)=c(names(ci),paste('all <|>',round(ie,4)))
  }
  print(res)
  res=list(effects=ab,vars=txt,sobel=m,ci.type=if(boot)'Bootstrap' else 'Permutation',ci=res)
  if(figure) medfig(res,...)
  invisible(res)
}

medfig=function(mat,ci=NULL,ci.type=NULL,xlab='x variable',ylab='y variable',mlab='mediating variable',
  title=FALSE,note=TRUE,box=TRUE,digits=3,note.sig=TRUE,fin.length=.5,cords.box=list(),cords.arrow=list(),
  padding.box=.05,padding.arrow=.02,padding.number=.02,padding.title=.1,padding.note=.1,arrow.scale=.01,
  box.fill=NA,center.adj=.1,color=c(line=NA,box=NA,label=NA,number=NA,title=NA,note=NA),
  weight=c(line=NA,box=NA,label=NA,number=NA,title=2,note=.9),save=FALSE,format=pdf,name='mediation',...){
  if(missing(mat)) mat=bspmed(disp,mpg,wt,scale(mtcars))
  if(is.list(mat) && 'sobel'%in%names(mat)){
    nm=names(mat)
    if('ci'%in%nm && missing(ci)) ci=mat$ci
    if('ci.type'%in%nm && missing(ci.type)) ci.type=mat$ci.type
    if('vars'%in%nm){
      nm=names(mat$vars)
      if('x'%in%nm && missing(xlab)) xlab=mat$vars$x
      if('y'%in%nm && missing(ylab)) ylab=mat$vars$y
      if('y'%in%nm && missing(mlab)) mlab=mat$vars$m
    }
    mat=mat$sobel
  }
  if(!is.null(ci) && !is.null(nrow(ci))) ci=ci[nrow(ci),,drop=FALSE]
  if(NCOL(mat)<3) stop('mat must have at least 3 values')
  pn=c('line','box','label','number','title','note')
  dop=par(no.readonly=TRUE)
  ea=list(...)
  do.call(par,c(list(mar=c(0,0,0,0),oma=c(0,0,0,0)),if(any(ons<-names(ea)%in%names(dop)))ea[ons]))
  on.exit(par(dop))
  if(!missing(color)){
    color[is.null(color) | color=='none']='NA'
    if(is.null(names(color))) if(length(color)==1) color[pn]=color else names(color)=pn[seq_along(color)]
    color=color[pn]
  }
  if(any(is.na(color))){
    color=color[!is.na(color)]
    color[pn[!pn%in%names(color)]]=par('col')
  }
  if(!missing(weight)){
    if(is.null(names(weight))) if(length(weight)==1) weight[pn]=weight else names(weight)=pn[seq_along(weight)]
    weight=weight[pn]
  }
  if(any(is.na(weight))){
    weight=weight[!is.na(weight)]
    mas=pn[!pn%in%names(weight)]
    if(any(tmas<-mas%in%pn[1:2])) weight[mas[tmas]]=par('lwd')
    if(any(tmas<-mas%in%pn[3:6])) weight[mas[tmas]]=par('cex')
  }
  tn=c(
    title=!missing(title) && (!is.logical(title) || title),
    note=missing(note) || (!is.logical(note) || note)
  )
  lo=matrix(c(2,padding.title,1,1,3,padding.note),2)[,c(tn[['title']],TRUE,tn[['note']]),drop=FALSE]
  if(!tn[['title']] && tn[['note']]) lo[1,2]=2
  layout(lo[1,],heights=lo[2,])
  plot.new()
  colnames(mat)=c('a','b','c',"c'",'i')[seq_len(ncol(mat))]
  cn=colnames(mat)
  txt=list(x=xlab,m=mlab,y=ylab)
  sl=vapply(txt,strwidth,0)/max(1,3-weight[['label']])+padding.box/2*weight[['label']]
  sp=list(x=c(.1,.1),m=c(.5,.9),y=c(.9,.1))
  if(!missing(cords.box)){
    if(is.null(names(cords.box))) names(cords.box)=c('x','m','y')[seq_along(cords.box)]
    sp=c(cords.box,sp[!names(sp)%in%names(cords.box)])
  }
  bx=list()
  for(n in c('x','y','m')){
    bx[[n]]=list(max(0,sp[[n]][1]-sl[n]),max(0,sp[[n]][2]-padding.box),
      min(1,sp[[n]][1]+sl[n]),min(1,sp[[n]][2]+padding.box))
    if(box) do.call(graphics::rect,c(bx[[n]],col=box.fill,border=color[['box']],lwd=weight[['box']]))
    do.call(graphics::text,c(as.list(sp[[n]]),txt[[n]],col=color[['label']],cex=weight[['label']]))
  }
  aco=list(
    a=list(sp$x[1]+padding.arrow/1.5,bx$x[[4]]+padding.arrow*1.5,bx$m[[1]]-padding.arrow/1.5,bx$m[[2]]-padding.arrow*1.5),
    b=list(bx$m[[3]]+padding.arrow/2,bx$m[[2]]-padding.arrow,sp$y[1]-padding.arrow/1.5,bx$y[[4]]+padding.arrow*1.5),
    c=list(bx$x[[3]]+padding.arrow/1.5,sp$x[2],bx$y[[1]]-padding.arrow,sp$y[2])
  )
  if(!missing(cords.arrow) && is.list(cords.arrow) && any(names(cords.arrow)%in%names(aco)))
    for(n in names(cords.arrow)) if(length(cords.arrow[[n]])==4 && n%in%names(aco))
      aco[[n]]=cords.arrow[[n]]
  d=c(((u<-par('usr'))[2]-u[1])/(p<-(p<-par('pin'))/max(p))[1],(u[4]-u[3])/p[2])
  s=function(x0,y0,x1,y1) atan((y1-y0)/(x1-x0)*d[1]/d[2])/pi*180
  la=weight[['line']]/2/100
  adj=padding.number+la/2
  adj=list(a=c(-adj,adj),b=c(adj,adj),c=c(0,-adj-la*5))
  mat=vapply(as.data.frame(mat),function(p) c(round(p[1],digits),p[2],
    c('***','**','*','\u2020','italic(ns)')[which(p[2]<c(.001,.01,.05,.1,Inf))[1]]),character(3))
  op=function(sl){
    s=strsplit(sl,'')[[1]][1]==c('*','\u2020','i')
    paste0(c('*"','^"','^')[s],sl,c('"','"','')[s])
  }
  ct=function(m,n) if(note.sig) paste0('expression(',m[1,n],op(as.character(m[3,n])),if(n=='c')
    paste0('~(',m[1,"c'"],op(as.character(m[3,"c'"])),')'),')') else
      paste0('expression(',m[1,n],if(n=='c')paste0('~(',m[1,"c'"],')'),')')
  ar=function(l,a){
    do.call(graphics::segments,c(l,col=color[['line']],lwd=weight[['line']]))
    l=unlist(l)
    p=matrix(c(l[3],l[4],l[3]-arrow.scale*fin.length,l[4]+arrow.scale,l[3]+arrow.scale,
      l[4],l[3]-arrow.scale*fin.length,l[4]-arrow.scale),2)
    cen=matrix(rep(c(l[3],l[4]),4),2)
    a=a/180*pi
    p=matrix(c(cos(a),sin(a),-sin(a),cos(a)),2)%*%(p-cen)*d+cen
    if(l[1]>l[3]) p[,2:4]=p[,2:4]+(p[,1]-p[,2:4])*2
    polygon(p[1,],p[2,],col=color[['line']],lwd=weight[['line']])
  }
  for(n in names(aco)){
    co=aco[[n]]
    an=do.call(s,co)
    do.call(ar,list(co,an))
    text(mean(c(co[[1]],co[[3]]))+adj[[n]][1],mean(c(co[[2]],co[[4]]))+adj[[n]][2],
      eval(parse(text=ct(mat,n))),srt=an,col=color[['number']],cex=weight[['number']])
  }
  if('i'%in%cn){
    it=ct(mat,'i')
    if(!is.null(ci)){
      l=NA
      if(!is.null(names(ci))) l=as.numeric(gsub('[^0-9.]','',names(ci)[1]))
      l=if(is.na(l)) 'CI' else paste0((if(l>1) 100 else 1)-l*2,'% CI')
      ci[1:2]=round(ci[1:2],digits)
      it=sub(')$',paste0('*", ',l,' [',ci[[1]],', ',ci[[2]],']','")'),it)
    }
    text(mean(c(mean(c(bx$x[[3]],bx$y[[1]])),sp$m[1])),
      mean(c(mean(c(bx$x[[4]],bx$y[[4]])),bx$m[[2]]))-center.adj,eval(parse(text=it)),
      col=color[['number']],cex=weight[['number']])
  }
  if(tn[['title']]){
    plot.new()
    text(.5,.5,if(is.character(title)) title else paste('Effect of',xlab,'on',ylab,'by way of',ylab),
      col=color[['title']],cex=weight[['title']])
  }
  if(tn[['note']]){
    plot.new()
    text(.5,.5,if(is.character(note)) note else
      eval(parse(text=paste0('expression(paste(',paste(
        if(!is.null(ci.type)) paste('"',ci.type,'confidence intervals; ",'),
        "p < .1^'\u2020',', ', p < .05*'*',', ', p < .01*'**',', ', p < .001*'***'",'))')))),
      col=color[['note']],cex=weight[['note']])
  }
  if(save || !missing(format) || !missing(name)) tryCatch({
    t=as.character(substitute(format))
    if(length(t) != 1) t = t[length(t)]
    tt=if(grepl('cairo', t, TRUE)){
      paste0('.', if(grepl('_', t, fixed = TRUE)) strsplit(t, '_')[[1]][2] else sub('Cairo', '', t, fixed = TRUE))
    }else if(t=='postscript') '.ps' else paste0('.', t)
    dims=if(grepl('jpeg|png|tiff|bmp|bit', t)) dev.size(units='px') else dev.size()
    fn=paste0(name, tolower(tt))
    dev.copy(format,fn,width=dims[1],height=dims[2])
    dev.off()
    message('image saved: ',getwd(),'/',fn)
  },error=function(e)warning('unable to save image: ',e$message,call.=FALSE))
  invisible(list(box.width=sl,box.center=sp,box.sides=bx,lines=aco))
}

lmp=function(m,eff=NULL,devch=F){
  s=summary(m)
  if(missing(eff) || is.null(eff)){
    s$coefficients=cbind(s$coef,p=2*(1-pnorm(abs(s$coef[,3]))))
  }else{
    if(!any(grepl(eff,formula(m),fixed=TRUE))) stop(eff," was not found in m's formula")
    if(!'pbkrtest'%in%installed.packages()) install.packages('pbkrtest')
    print(pbkrtest::KRmodcomp(m,update(m,formula(paste('~.-',eff)))))
    if(devch) cat('\n')
  }
  if(devch){
    l=ncol(s$coef)
    ll=logLik(m)
    dc=c(NA,vapply(rownames(s$coef)[-1],function(v)
      ll-logLik(update(m,formula(paste('~.-',v))))
      ,0))
    s$coefficients=cbind(s$coef,devch=dc,devchP=1-pchisq(2*dc,1))
  }
  if(devch || missing(eff)) s
}

chtest=function(m,term){
  y=colnames(m@frame)[1]
  if(missing(term)){
    term=colnames(m@frame)[2]
    message('tests for inclusion of ',term)
  }
  null=update(m,paste('~.-',gsub('[+*]','-',gsub('^.*~ | \\+ \\(.*','',
    paste(deparse(m@call$formula),collapse=' ')))))
  sm=update(m,paste('~.-',term))
  c(
    AIC=AIC(sm)-AIC(m),
    BIC=BIC(sm)-BIC(m),
    Psudo_R2=psrs(null,m),
    Psudo_R2_change=psrs(sm,m)
  )
}

psrs=function(m0,ma){
  1-(sum(as.data.frame(VarCorr(ma))$vcov)/sum(as.data.frame(VarCorr(m0))$vcov))
}

bsci=function(m,iter=10000,alpha=.05){
  su=seq_len(nrow(m$model))
  t(apply(vapply(seq_len(iter),function(i)
    update(m,data=m$model[sample(su,replace=TRUE),])$coef,m$coef)
    ,1,quantile,c(alpha/2,1-alpha/2)))
}

permci = function(m, iter = 10000, alpha = .05){
  su=seq_len(nrow(m$model))
  t(apply(vapply(seq_len(iter),function(i){
    td = m$model
    td[,1] = sample(td[,1])
    update(m, data = td)$coef
  },m$coef),1,quantile,c(alpha/2,1-alpha/2)))
}

convert_effect = function(effect, type = 'r', N = 3, k = 2, adjust_f = TRUE, display_f = FALSE, print = TRUE){
  type = tolower(substr(sub('(cohen|pearson).* ', '', type, TRUE), 1, 1))
  df1 = k - 1
  df2 = N - k
  if(adjust_f && type == 'f' && !missing(N)) effect = sqrt(df1 * effect / df2)
  d = switch(type,
    r = (2 * effect) / sqrt(1 - effect ^ 2),
    d = effect,
    a = sqrt(2) * qnorm(effect),
    o = log(effect) * sqrt(3) / pi,
    e = sqrt(effect / (1 - effect)) * 2,
    f = effect * 2
  )
  f = d / 2
  op = as.data.frame(cbind(
    "Pearson's r" = d / sqrt((d ^ 2) + 4),
    "Cohen's d" = d,
    'AUC' = pnorm(d / sqrt(2)),
    'Odds Ratio' = exp(d * pi / sqrt(3)),
    'eta^2' = f ^ 2 / (1 + f ^ 2),
    "Cohen's f" = f
  ))
  if(display_f || (!missing(N) && missing(display_f))){
    op$`F` = f ^ 2 / df1 * df2
    op$`num_df` = df1
    op$`den_df` = df2
    op$`p-value` = pf(abs(op$`F`), df1, df2, lower.tail = FALSE)
  }
  if(print) print(op, digits = 4, row.names = FALSE)
  invisible(op)
}

critical_effect = function(N, k = 2, alpha = .05) convert_effect(qf(1 - alpha, k - 1, N - k), 'f', N, k)

parapower = function(effect = NULL, type = 'f', N = NULL, k = 2, alpha = .05, power = .8, range){
  ck = c(
    e = is.null(effect), N = is.null(N),
    power = missing(power) || is.null(power),
    alpha = missing(alpha) || is.null(alpha)
  )
  if(!any(ck)){
    ck['e'] = TRUE
    warning('estimating effect, since n, alpha, and power were all supplied')
  }
  if(ck['e'] && ck['N']){
    effect = c(.3, .5, .7)
    type = 'd'
    ck['e'] = FALSE
  }else if(ck['N'] && (!ck['power'] || !ck['alpha'])){
    N = 50
    ck['N'] = FALSE
  }
  if(is.null(alpha) && is.null(power)){
    alpha = .05
    ck['alpha'] = FALSE
    warning('setting alpha to .05, since power was set to NULL')
  }else if(is.null(alpha)) ck['power'] = FALSE
  if(missing(range)) range = if(ck['N']) c(2, 1e4) else c(0, if(!ck['e']) 1 else 100)
  par = names(which(ck))[1]
  pars = list(effect = effect, N = N, alpha = alpha, power = power)
  l = vapply(pars, length, 0)
  ll = if(all(l == max(l))) l[par] else which.max(l)
  l = max(l)
  for(e in seq_along(pars)) if(l > length(pars[[e]]))
    pars[[e]] = rep_len(if(is.null(pars[[e]])) NA else pars[[e]], l)
  op = as.data.frame(matrix(0, l, 15, dimnames = list(
    paste(names(ll), '=', pars[[ll]]), c(
      "n", 'k', 'N', "alpha", "power", "Pearson's r", "Cohen's d", "AUC", "Odds Ratio",
      "eta^2", "Cohen's f", 'F', 'num_df', 'den_df', 'p-value'
    ))))
  fun = function(i){
    if(par == 'N') N = i else if(par == 'e') f = i else if(par == 'power') p = i else a = i
    df2 = (N - 1) * k
    pf(qf(1 - a, k - 1, df2), k - 1, df2, f ^ 2 * N * k, lower.tail = FALSE) - p
  }
  for(set in seq_len(l)){
    N = pars$N[set]
    f = if(!ck['e']) convert_effect(pars$e[set], type, print = FALSE)[["Cohen's f"]] else NULL
    a = pars$a[set]
    p = pars$p[set]
    opt = if(par == 'power') fun(0) else uniroot(fun, range)$root
    if(par == 'N') N = opt else if(par == 'e') f = opt else if(par == 'power') p = opt else a = opt
    op[set,] = c(0, k, N, a, p, convert_effect(f, 'f', N, k, adjust_f = FALSE, print = FALSE))
    if(!ck['e']) op[set, par] = if(ck['N']) round(opt + .499) else opt
  }
  op$n = op$N
  op$N = op$n * k
  print(op, digits = 4, row.names = FALSE)
  invisible(op)
}

alphas=function(m){
  m=na.omit(m)
  k=ncol(m)
  r=mean(cor(m)[lower.tri(diag(k))])
  c(
    raw=k/(k-1)*(1-sum(apply(m,2,var))/var(rowSums(m))),
    standardized=k*r/(1+(k-1)*r)
  )
}

pcor = function(...){
  m = -cov2cor(solve(cov(...)))
  diag(m) = 1
  m
}

catcomp=function(a,b){
  all=list(a=a[!a%in%b],both=a[a%in%b],b=b[!b%in%a])
  l=rep('',max(vapply(all,length,0)))
  as.data.frame(sapply(all,function(wl){o=l;o[seq_along(wl)]=wl;o}))
}

confscores = function(predicted, actual, percent = TRUE){
  if(length(predicted) != length(actual)) stop('predicted and actual vectors are not the same length')
  m = table(actual, predicted)
  cl = sort(unique(actual))
  precision = recall = structure(numeric(length(cl)), names = cl)
  tv = colSums(m)
  su = names(which(tv != 0 & names(tv) %in% rownames(m) & names(tv) %in% cl))
  if(length(su)) precision[su] = diag(m[su, su]) / tv[su]
  tv = rowSums(m)
  su = names(which(tv != 0 & names(tv) %in% colnames(m) & names(tv) %in% cl))
  if(length(su)) recall[su] = diag(m[su, su]) / tv[su]
  F1 = 2 * precision * recall / (precision + recall + .00001)
  scores = list(precision = precision, recall = recall, F1 = F1)
  list(
    scores = cbind(
      n = c(rowSums(m), overall = length(actual)),
      predicted = c(vapply(cl, function(l) sum(predicted == l), 0), sum(predicted %in% cl)),
      rbind(
        do.call(base::cbind, scores),
        unlist(lapply(scores, mean, na.rm = TRUE))
      )
    ),
    accuracy = mean(predicted == actual),
    confusion = if(percent) t(t(m) / colSums(m)) else m
  )
}

fractal=function(proc=z^2+c,e=25,size=min(dev.size(units='px')),rr=c(-1.8,0.6),ir=c(-1.2,1.2),
  col=grey(0:255/270),save=FALSE,name='fractal',delay=30,full=FALSE,...){
  f=full || any(!missing(save),!missing(name),!missing(delay))
  c=matrix(complex(
    real=rep(seq(rr[1],rr[2],length.out=size),each=size),
    imaginary=rep(seq(ir[1],ir[2],length.out=size),times=size)
  ),size,size)
  z=0
  x=array(0,c(size,size,ifelse(f,e,1)))
  for(j in 1:e){
    z=eval(substitute(proc))
    if(f) x[,,j]=exp(-abs(z)) else x=exp(-abs(z))
  }
  op=par(mar=c(0,0,0,0),oma=c(0,0,0,0))
  image(if(f) x[,,e] else x,axes=FALSE,col=col)
  par(op)
  if(any(save,!missing(name),!missing(delay))){
    if(!'caTools'%in%installed.packages()) install.packages('caTools')
    require(caTools,NULL,TRUE)
    write.gif(x,paste0(name,'.gif'),col=col,delay=delay,...)
  }
  invisible(x)
}

matrixplot=function(m, type='character', colors=c('#ffffff',splot.color()), axes=FALSE,
  xlab = NA, ylab = NA, ...){
  d=dim(m)
  ind=seq_len(d[2])
  args=list(...)
  if(!'mar'%in%names(args)) args$mar=c(0,0,0,0)
  op=par(args[names(args)%in%names(par(no.readonly=TRUE))])
  on.exit(par(op))
  if(grepl('^ch',type,TRUE)){
    plot(NA,ylim=c(1,d[1]),xlim=c(1,d[2]),axes=axes,col=colors,ylab=ylab,xlab=xlab,...)
    for(r in seq_len(d[1])) points(ind,rep(r,d[2]), pch=m[d[1]-r+1,])
  }else{
    m=matrix(as.numeric(as.factor(m)),dim(m)[1])
    image(m,axes=axes,col=colors,...)
  }
}

enlarge=function(m,times=10){
  od=dim(m)
  d=od*times
  s=seq_len(times)
  em=matrix(0,d[1],d[2])
  for(r in seq_len(od[1])) for(c in seq_len(od[2])) em[s+times*(r-1),s+times*(c-1)]=m[r,c]
  em
}

schelling = function(n = 138, dims = c(13, 16), probs = c(o = .5, '#' = .5), range = 1, ideals = .5,
  tols = .1, dist = Inf, concern = 'own', ..., distweight = .1, shuffle = TRUE, maxepochs = 99,
  idealtype = 'min', moveto = 'nearest', record = FALSE, plot = TRUE, mfrow = c(1, 2), colors = NULL,
  gif = FALSE, delay = 20, magnify = 60){
  sense = function(p, op){
    g = m[op]
    m[op] = ''
    r = gtp[[4]][[g]]
    pr = arrayInd(p, .dim = dims)
    d = lapply(1:2, function(i) seq(max(1, pr[i] - r), min(dims[i], pr[i] + r)))
    m = m[d[[1]], d[[2]]]
    m = m[m != '']
    if(length(m) != 0) (if(gtp[[1]][[g]] < 1) mean else sum)(m == gtp[[8]][[g]]) else 0
  }
  move = function(p){
    g = m[p]
    cv = sense(p, p)
    if(ck[[gtp[[7]][[g]]]](cv, g = g)){
      pr = arrayInd(p, .dim = dims)
      md = min(max(max(abs(pr - dims)), max(abs(pr - c(0, 0)))), gtp[[5]][[g]])
      r = if(moveto) 0 else md - 1
      while(r < md){
        r = r + 1
        d = lapply(1:2, function(i) seq(max(1, pr[i] - r), min(dims[i], pr[i] + r)))
        i = as.numeric(index[d[[1]], d[[2]]])
        pp = i[which(m[i] == '')]
        if(length(pp)){
          ds = vapply(pp, arrayInd, c(0, 0), .dim = dims)
          ds = abs(pr[1] - ds[1,]) * abs(pr[2] - ds[2,]) + 1
          ds = ds / max(ds)
          pv = vapply(pp, sense, 0, p)
          if(ck[[gtp[[7]][[g]]]](pv, ds, cv, g = g)){
            m[pp[ck[[gtp[[7]][[g]]]](pv, ds, cv, TRUE, g)]] <<- g
            m[p] <<- ''
            if(gif) complrec <<- c(complrec, list(m))
            break
          }
        }
      }
    }
  }
  if(!missing(delay) || !missing(magnify)) gif = TRUE
  if(gif) record = TRUE
  moveto = grepl('^n', moveto, TRUE)
  idealtype = tolower(idealtype)
  idealtype[grepl('^ma', idealtype)] = '1'
  idealtype[grepl('^mi', idealtype)] = '2'
  idealtype[!idealtype%in%c(1,2)] = '0'
  ck = list(
      '1' = function(v, d = 0, cv = 0, rm = FALSE, g){
        r = v + d * gtp[[6]][[g]]
        if(rm) which.min(r) else any(if(missing(cv)) r > gtp[[1]][[g]] else r < cv)
      },
      '2' = function(v, d = 0, cv = 0, rm = FALSE, g){
        r = v - d * gtp[[6]][[g]]
        if(rm) which.max(r) else any(if(missing(cv)) r < gtp[[1]][[g]] else r > cv)
      },
      '0' = function(v, d = 0, cv = 0, rm = FALSE, g){
        r = abs(v - gtp[[1]][[g]]) + d * gtp[[6]][[g]]
        if(rm) which.min(r) else any(r > gtp[[2]][[g]])
      }
  )
  gtp = list(ideals, tols, probs, range, dist, distweight, idealtype, concern)
  ngroups = max(vapply(gtp, length, 0))
  gns = unique(do.call(base::c, lapply(gtp, names)))
  if(length(gns) != ngroups && missing(probs)){
    gtp[[3]] = unname(probs)
    gns = unique(do.call(base::c, lapply(gtp, names)))
  }
  if(length(gns) != ngroups) gns = if(ngroups < 27) LETTERS[seq_len(ngroups)] else paste0('g', seq_len(ngroups))
  for(v in seq_along(gtp)){
    if(length(gtp[[v]]) != ngroups) gtp[[v]] = rep_len(gtp[[v]], ngroups)
    if(is.null(names(gtp[[v]]))) names(gtp[[v]]) = gns
  }
  gtp[[8]][!gtp[[8]]%in%gns] = names(gtp[[8]][!gtp[[8]]%in%gns])
  ps = dims[1] * dims[2]
  index = matrix(seq_len(ps), dims[1], dims[2])
  n = if(n < 1) round(ps * n) else min(round(ps * .95), n)
  m = matrix(sample(c(rep('', ps - n), sample(gns, n, TRUE, probs))), dims[1], dims[2])
  ms = complrec = list(m)
  e = 1
  same = 0
  while(e < maxepochs){
    for(i in if(shuffle) sample(which(m != '')) else which(m != '')) if(m[i] != '') move(i)
    if(e > 1 && all(ms[[if(record) e else 2]] == m)){
      same = same + 1
      if(same > 1) break
    }
    e = e + 1
    ms[[if(record) e else 2]] = m
  }
  if(is.null(colors) && (plot || gif)) colors = c('#ffffff', rev(splot.color(255, 'grey', extend = 0)))
  if(plot){
    op=par(mfrow = mfrow)
    on.exit(par(op))
    matrixplot(ms[[1]], col = colors, main = 'Initial Population', ..., mar = c(0.2, 0.2, 1.2, 0.2))
    matrixplot(ms[[if(record) e else 2]], col = colors, main = paste('After',e,'Epochs'),
      ..., mar = c(0.2, 0.2, 1.2, 0.2))
  }
  if(gif){
    e = length(complrec)
    imgs = array(0, c(dims[1] * magnify, dims[2] * magnify, e))
    for(i in seq_len(e)) imgs[,,i] = enlarge(matrix((as.numeric(as.factor(complrec[[i]])) - 1) * 100, dims[1]), magnify)
    caTools::write.gif(imgs, 'schelling.gif', col = colors, delay = delay)
  }
  invisible(list(complete = complrec, areas = ms, epochs = e))
}

# function to make composite measures
comp=function(data,items=NULL,weight=FALSE,factor=1,center=TRUE,scale=TRUE,print=TRUE){
  cw=missing(weight) || (!missing(weight) && is.logical(weight) && weight)
  if(class(items)=='bscfa'){
    items=na.omit(items$coef[,grep(paste0('F.',factor),colnames(items$coef))])
    if(cw) weight=apply(items,2,mean)
    items=sub('^[^>]*> ','',colnames(items))
  }else if(is.list(items) && 'stdcoef'%in%names(items)){
    items=items$stdcoef
    items=items[grep(paste0('<--- F.',factor),items[,3]),]
    if(cw) weight=items[,2]
    items=sub(' .*','',items[,3])
  }
  if(is.null(items)){
    items=c('i','shehe','auxverb','adverb','conj','article','prep','quant')
    if(cw || (missing(weight) && missing(factor))) weight=c(1,1,1,1,1,-1,-1,-1)
  }
  if(is.logical(weight)) weight=rep(1,length(items))
  data=tryCatch(data[,items]
    ,error=function(e)stop('could not find the specified items in your data',call.=FALSE))
  if(center) data=scale(data,scale=scale)
  data=tryCatch(apply(data,1,function(r)mean(r*weight))
    ,error=function(e)stop('could not apply provided weight to your variables',call.=FALSE))
  if(print) print(data.frame(cbind(items,weight)))
  data
}

percomp=function(d) data.frame(
  extroversion=comp(d,c('family','friend','posemo','sexual','social','we'),print=FALSE),
  emotionalStability=comp(d,c('anger','anx','i','negemo','sad'),c(-1,-1,-1,-1,-1),print=FALSE),
  agreeableness=comp(d,c('anger','family','negate','posemo','swear','time','we'),c(-1,1,-1,1,-1,1,1),print=FALSE),
  conscientiousness=comp(d,c('anger','negate','negemo','swear','time'),c(-1,-1,-1,-1,1),print=FALSE),
  openness=comp(d,c('article','death','family','home','pronoun','time'),c(1,1,-1,-1,-1,-1),print=FALSE)
)

topics = function(dtm,topics=10,words=20,cutoff=.01,method='VEM',type=LDA){
  if(!require('topicmodels')){
    install.packages('topicmodels')
    library('topicmodels')
  }
  if(class(dtm)%in%c('character','factor')) dtm=lma_dtm(dtm,'function',dc.min=2)
  if(substitute(type)=='LDA'&&missing(method)) method='Gibbs'
  m=(type)(dtm,topics,method)
  tms=terms(m,words,threshold=cutoff)
  if(!is.list(tms)) tms = as.list(as.data.frame(tms))
  list(model=m,dict=tms,scores=lma_weight(lma_termcat(dtm,tms),percent=TRUE))
}

facex = function(loads, data, nitems, overlap_penalty = .01){
  m = cbind(loads, loads * -1)
  k = seq_len(ncol(m) / 2)
  is = seq_len(ncol(m))
  cn = rownames(loads)
  if(is.null(cn) || !all(cn %in% colnames(data))){
    cn = colnames(data)
    if(ncol(data) != NROW(loads))
      stop('!all(rownames(loads) %in% colnames(data)) or ncol(data) != NROW(loads)')
  }
  nr = length(cn)
  nitems = min(if(missing(nitems)) round(length(cn) / 10) else nitems, nr)
  for(r in seq_len(nr)){
    ro = m[r, k]
    ri = m[r, k + 2]
    m[r, c(ro < max(ro), ri < max(ri))] = 0
  }
  items = as.list(is)
  alpha = is
  for(i in is){
    col = m[, i]
    cv = sort(col, decreasing = TRUE)[nitems]
    su = col > cv
    su[is.na(su)] = FALSE
    items[[i]] = cn[if(sum(su)) su else col >= cv]
    alpha[i] = alphas(data[, items[[i]]])[2]
  }
  t1 = which.max(alpha)
  t1i = items[[t1]]
  t1l = length(t1i)
  is = is[-t1]
  sel = c(t1, is[which.max(vapply(is, function(i)
    alpha[i] - sum(t1i %in% items[[i]]) * overlap_penalty
  , 0))])
  list(
    items = items[sel],
    alphas = alpha[sel],
    scores = vapply(items[sel], function(ts) rowSums(data[, ts]), numeric(nrow(data)))
  )
}

# function for CFAs
afa=function(items,data,fitinds=c('srmr','rmsea','tli','cfi','chisq','df','pvalue'),...){
  if(!'lavaan'%in%installed.packages()) install.packages('lavaan')
  if(is.null(names(items))) names(items)=paste0('f',seq_along(items))
  ms=names(items)
  mods=res=alpha=list()
  measuresRes=data.frame(matrix(NA,length(ms),length(fitinds)+2,
    dimnames=list(ms,c('items','factors',fitinds))))
  for(m in ms){
    cat('\nRefining',m,'items...\n')
    tm=afa.model(data[,items[[m]]],...)
    if(is.null(tm)) next else tm=tm$factors
    cml=c()
    for(s in seq_along(tm)){
      cml=c(cml,paste0(m,s))
      k=length(tm[[s]])
      r=mean(cor(na.omit(data[,tm[[s]]]))[lower.tri(diag(k))])
      alpha[[m]]=c(alpha[[m]],k*r/(1+(k-1)*r))
      mods[[paste0(m,s)]]=paste0(m,s,'=~ ',paste0(tm[[s]],collapse=' + '))
    }
    res[[m]]=lavaan::cfa(paste(mods[cml],collapse='\n'),data,missing='ML')
  }
  if(length(res)==0) return(NULL)
  nd=data.frame(do.call(base::cbind,
    lapply(res,function(m)tryCatch(lavaan::lavPredict(m,newdata=data),error=function(e)NULL))
  ))
  for(m in seq_along(res)){
    cm=lavaan::lavInspect(res[[m]])$lambda
    measuresRes[m,]=c(nrow(cm),ncol(cm),
      tryCatch(lavaan::fitMeasures(res[[m]],fitinds),error=function(e)rep(NA,length(fitinds))))
  }
  k=max(vapply(alpha,length,0))
  alphaRes=matrix(0,length(alpha),k,dimnames=list(names(alpha),paste0('F',seq_len(k))))
  for(m in names(alpha)) alphaRes[m,seq_along(alpha[[m]])]=alpha[[m]]
  alphaRes=format(data.frame(alphaRes),digits=3,zero.print='')
  measuresRes=format(measuresRes,digits=3)
  cat('\nSubscale alphas:\n')
  print(alphaRes)
  cat('\nFit indices:\n')
  print(measuresRes)
  list(data=nd,model=mods,alpha=alphaRes,fit=measuresRes)
}

# function for EFAs
afa.model=function(x,nfactors,samples,size,criteria='parallel',cfi.change=.01,cutoff=.35,
  cutoff.percent,minvars=2,iter=100,rotation='promax',replace=TRUE,complete=FALSE,print=TRUE){
  if(ncol(x)<3) stop('x must have at least 3 columns',call.=FALSE)
  par=grepl('p|r|e',criteria,TRUE)
  su=complete.cases(x)
  if(!any(su)){
    x=x[,apply(x,2,function(c)any(!is.na(c)))]
    if(nrow(x)<1) stop('too many missing data',call.=FALSE)
    su=complete.cases(x)
  }
  tx=apply(x[su,],2,function(c)as.numeric(factor(c)))
  r=sum(su)
  p=ncol(tx)
  ndf=p*(p-1)/2
  vs=colnames(tx)
  if(missing(nfactors)) nfactors=max(1,round(p+.5*(1-(p*8)^.5)-.41))
  size=if(missing(size)) round(r/6) else min(r,size)
  k=if(missing(samples)) round(r/size) else samples
  sf=matrix(1,3,iter,dimnames=list(c('CFI','Parallel','Kaiser')))
  success=0
  fc=matrix(0,p,p,dimnames=list(vs,vs))
  fl=matrix(NA,iter,length(vs),dimnames=list(c(),vs))
  fsq=seq_len(nfactors)
  time=proc.time()[3]
  if(print) cat('Resampling: 0 /',iter)
  for(i in seq_len(iter)){
    fm=NULL
    su=lapply(seq_len(k),function(s)sample(r,size,replace))
    tr=matrix(0,nfactors,k)
    et=tryCatch(apply(vapply(
      c(lapply(su,function(s)cor(tx[s,])),list(cor(matrix(rnorm(round(r/k)*p),round(r/k)))))
      ,function(c)eigen(c-solve(diag(diag(solve(c)))))$values,numeric(p)
    ),1,function(r)c(sum(r[k+1]<r[-(k+1)])>k/2,sum(r[-(k+1)]>0)>k/2))[,fsq,drop=FALSE]
      ,error=function(e)NULL)
    sf[2:3,i]=if(!is.null(et)) apply(et,1,function(r)max(c(1,which(r)))) else c(1,1)
    for(f in fsq){
      m=tryCatch(lapply(su,function(s)factanal(tx[s,],f,rotation=rotation)),
        error=function(e)NULL)
      if(is.null(m)) next
      tr[f,]=vapply(m,function(sm){
        cm=sm$cor%*%diag(p)
        cm=sm$n.obs*(sum(diag(cm))-log(det(cm))-p)
        ((cm-ndf)-(ifelse(is.null(sm$S),0,sm$S)-sm$dof))/(cm-ndf)
      },0)
      cfick=all(tr[f,]>tr[f-1,]+cfi.change)
      if(f==1 || cfick) sf[1,i]=f
      if(is.null(fm) || ifelse(par,f==sf[2,i],cfick)){
        fm=m
        if(!complete && f>1) break
      }
    }
    if(is.null(fm)) next
    if(ncol(fm[[1]]$l)==1){
      rl=lapply(fm,function(m)list(names(m$l[which(abs(m$l)>cutoff),1])))
    }else{
      rl=lapply(fm,function(m)apply(m$l,1,function(r){r[-which.max(abs(r))]=0;r}))
      rl=lapply(rl,function(m)apply(m,1,function(c)list(names(which(abs(c)>cutoff)))))
    }
    fs=list()
    for(n in vs){
      ind=sapply(rl,function(s)lapply(s,function(l)sum(n%in%l[[1]])))
      if(sum(unlist(ind))==k){
        ind=if(class(ind)=='list') rep(1,k) else apply(ind,2,which.max)
        tl=c()
        for(l in seq_along(rl)) tl=c(tl,unlist(rl[[l]][[ind[l]]]))
        tl=sort(names(which(table(tl)==k)))
        if(length(tl)>1) fs[[paste(tl,collapse='|')]]=tl
      }
    }
    for(l in fs) fc[l,l]=fc[l,l]+1
    fl[i,]=rowMeans(vapply(fm,function(s)apply(s$l,1,max),numeric(p)))
    success=success+1
    if(print) cat('\rResampling:',i,'/',iter)
  }
  time=proc.time()[3]-time
  if(print) cat(paste(
    '\rCompleted',success,'of',iter,'resamples in',
    round(ifelse(time>60,time/60,time),2),ifelse(time>60,'minutes.','seconds.')
  ))
  if(success<2) return(NULL)
  co=sf[ifelse(par,2,1),]
  co=ifelse(missing(cutoff.percent),ifelse(median(co)>1,1/mean(co),.6),cutoff.percent)
  cfs=mosf(fc/iter,co,minvars)
  if(length(cfs$factors)==0){
    cfs=mosf(fc/iter,co,2)
    if(length(cfs$factors)==0){
      co=.001
      su=colMeans(fl)>cutoff
      su[is.na(su)] = FALSE
      if(sum(su)<2) su=FALSE
      fc[!su,]=fc[,!su]=0
      cfs=mosf(fc/iter,co)
    }
  }
  tm=matrix(0,nfactors,3,dimnames=list(fsq))
  st=lapply(split(sf,1:3),function(r)(table(r)/iter)*100)
  for(i in seq(st)) tm[names(st[[i]]),i]=st[[i]]
  tm=data.frame(matrix(paste0(tm,'%'),nfactors,3,dimnames=list(paste(fsq,'Factors'))))
  colnames(tm)=c(paste0('CFI (',cfi.change,')'),'Parallel','Keiser')
  if(print){
    psq=seq_len(p)
    dimnames(cfs$full)=list(paste0(psq,'. ',colnames(cfs$full)),psq)
    cat(
      '\n\nRecommended number of factors (using ',
      ifelse(grepl('cfi',criteria,TRUE),'CFI','parallel'),
      ' for selection):\n\n'
      ,sep='')
    print(tm)
    cat(
      '\n\nConverged upon factors (trimming at ',round(co,2)*100,'%)',
      ':\n\nPercent of resamples in which variable set appeared\nwith loadings over ',cutoff,
      ' in all ',k,' samples.\n\n'
      ,sep='')
    print(cfs$full)
  }
  invisible(list(
    loadings=fl,
    nfactors.cfi=sf[1,],
    nfactors.par=sf[2,],
    counts=fc,
    summary.nfactors=tm,
    summary.counts=cfs$full,
    factors=cfs$factors
  ))
}

# function to pull factors from a matrix
mosf=function(x,cutoff,minvars=2,goal=max){
  x=data.frame(x)
  x[x<cutoff]=0
  o=do.call(order,-x)
  x=x[o,o]
  bound=function(x,h,goal){
    r=sum(x[h,])
    if(r<2) return(1)
    c=goal(colSums(x[,h:min(c(nrow(x),(h+r-1))),drop=FALSE]))
    if(c==r) return(c)
    r=goal(rowSums(x[h:min(c(nrow(x),(h+c-1))),,drop=FALSE]))
    goal(apply(x[,seq_len(r),drop=FALSE],2,sum))
  }
  f=list()
  h=1
  while(h<nrow(x)){
    fd=bound(x>0,h,goal)
    if(fd<minvars){h=h+fd;next}
    f[[length(f)+1]]=sort(rownames(x[h:min(c(nrow(x),(h+fd-1))),]))
    h=h+fd
  }
  list(full=format(x,digits=3,zero.print=''),factors=f)
}

code2name = function(codes){
  r = col2rgb(colors(TRUE))
  colnames(r) = colors(TRUE)
  s = as.data.frame(col2rgb(codes))
  colnames(s) = codes
  vapply(s, function(sc) names(which.min(colSums((sc - r) ^ 2))), '')
}

segment = function(x, k, bins = TRUE, bin_stat = mean, random = FALSE){
  l = length(x)
  g = if(random) sample(seq_len(k), l, TRUE) else rep(seq_len(k), each = as.integer(l / k))[seq_len(l)]
  if(bins) vapply(split(x, g), bin_stat, 0) else g
}

windowmean = function(x, len.out = 100, window_size = .5){
  n = length(x)
  ws = if(window_size < 1) n * window_size else min(round(n - len.out), window_size)
  if(n < len.out + ws) stop('x is too small for this len.out and window_size')
  s = max(1, round((n - (ws + 1)) / len.out))
  if(s * len.out > n - ws) s = s - 1
  w = seq_len(ws)
  vapply(c(0, seq_len(len.out - 2) * s, n - ws), function(ss) mean(x[w + ss]), 0)
}

costrans = function(x, k = 5, n = 100){
  l = length(x)
  o = numeric(n)
  s = (seq_len(l) - .5) * pi / l
  for(i in seq_len(k)) o[i] = sum(x * cos(s * (i - 1)))
  x = o[-1]
  b = o[1] * .5
  s = seq_len(n - 1) * pi / n
  for(i in seq_len(n)) o[i] = sum(x * cos(s * (i - .5)))
  o = (b + o) * 2 / n
}

circle = function(degrees = seq(1, 360, 1), radius = 1, center = c(0, 0)){
  radians = (90 + degrees) * pi / 180
  data.frame(
    x = radius * -cos(radians) + center[1],
    y = radius * sin(radians) + center[2]
  )
}

deal = function(x, co, shuffle = TRUE){
  x = as.data.frame(x)
  is = if(!is.null(colnames(x))) colnames(x) else seq_len(ncol(x))
  ix = seq_along(is)
  if(shuffle) ix = sample(ix)
  f = is[ix[1]]
  g = 1
  o = list(is[ix[1]])
  if(missing(co)) co = quantile(cor(x[, ix[1]], x[, -ix[1], drop = FALSE])[1,], .975, na.rm = TRUE)[[1]]
  for(i in ix[-1]){
    s = cor(x[, i], x[, f, drop = FALSE], 'complete.obs')[1,]
    if(all(is.na(s))) next
    if(max(s, na.rm = TRUE) < co){
      s = length(o) + 1
      o[[s]] = is[i]
    }else{
      s = g[which.max(s)]
      o[[s]] = c(o[[s]], is[i])
    }
    f = c(f, is[i])
    g = c(g, s)
  }
  o[order(vapply(o, length, 0), decreasing = TRUE)]
}

deal_mean = function(x, co, shuffle = TRUE){
  is = if(!is.null(colnames(x))) colnames(x) else seq_len(ncol(x))
  ix = seq_along(is)
  if(shuffle) ix = sample(ix)
  f = is[ix[1]]
  g = 1
  m = data.frame(v1 = x[, ix[1]])
  o = list(is[ix[1]])
  if(missing(co)) co = quantile(cor(x[, ix[1]], x[, -ix[1], drop = FALSE])[1,], .975, na.rm = TRUE)[[1]]
  for(i in ix[-1]){
    s = cor(x[, i], m, 'complete.obs')[1,]
    if(all(is.na(s))) next
    if(max(s, na.rm = TRUE) < co){
      s = length(o) + 1
      o[[s]] = is[i]
      m[, s] = x[, ix[i]]
    }else{
      s = g[which.max(s)]
      o[[s]] = c(o[[s]], is[i])
      m[, s] = rowMeans(x[, o[[s]]])
    }
    f = c(f, is[i])
    g = c(g, s)
  }
  o[order(vapply(o, length, 0), decreasing = TRUE)]
}

blocks = function(x, co, loadings = TRUE){
  ds = dim(x)
  nr = ds[2]
  if(ds[1] != nr) stop('x must be square')
  x = as.matrix(x)
  if(missing(co)) co = quantile(x, .966)
  ox = x
  x[x < co] = 0
  xf = as.data.frame(-x)
  colnames(xf) = NULL
  o = do.call(order, xf)
  x = x[o, o]
  bound = function(x, h){
    r = sum(x[h,])
    if(r < 2) return(1)
    c = max(colSums(x[, h:min(c(nr, h + r - 1)), drop = FALSE]))
    if(c == r) return(c)
    r = max(rowSums(x[h:min(c(nr, h + c - 1)), , drop = FALSE]))
    max(colSums(x[, seq_len(r), drop = FALSE]))
  }
  f = l = list()
  h = 1
  xbin = x > 0
  while(h < nr){
    fd = bound(xbin, h)
    if(fd < 2){
      h = h + fd
      next
    }
    su = h:min(c(nr, h + fd - 1))
    items = colnames(x[su, su])
    f = c(f, list(items))
    if(loadings) l = c(l, list(colMeans(ox[items, o])))
    h = h + fd
  }
  l = if(loadings){
    l = do.call(base::cbind, l)
    l = l[, order(colMeans(l), decreasing = TRUE)]
  }else NULL
  list(
    items = f,
    loadings = l
  )
}

itemex = function(loads, co, names, coperc = .975){
  if(is.null(loads[[1]]) || is.character(loads[[1]])) return(loads)
  dims = dim(loads)
  if(is.null(dims)){
    loads = matrix(loads, NROW(loads))
    dims = dim(loads)
  }
  if(missing(names))
    names = if(!is.null(rownames(loads))) rownames(loads) else seq_len(dims[1])
  if(missing(co) || !missing(coperc)) co = quantile(as.matrix(loads), coperc)
  lapply(seq_len(dims[2]), function(f){
    su = if(is.integer(co) || co > max(loads[, f]))
      order(loads[, f], decreasing = TRUE)[seq_len(min(co, dims[1]))] else loads[, f] > co
    names = names[su]
    names[order(loads[su, f])]
  })
}

taffyInf = function(m, k = 2){
  ks = seq_len(k)
  w = colSums(m)
  l = matrix(0, ncol(m), k, dimnames = list(colnames(m), ks))
  for(i in ks){
    h = if(i == 1) which.max(w) else which.max(w * -rowMeans(l))
    l[, i] = cor(m[, h], m)
  }
  l
}

taffy = function(m, k = nrow(m), minterm = 2, co = .975){
  w = colSums(m)
  ts = list()
  su = w != 0
  while(sum(su) && length(ts) < k){
    l = cor(m[, names(which.max(w[su]))], m[, su])[1,]
    f = names(which(l > quantile(l, co, na.rm = TRUE)))
    if(length(f) < minterm) break
    ts = c(ts, list(f))
    su[f] = FALSE
  }
  ts
}