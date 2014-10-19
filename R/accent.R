accent <-
function(x){
	# uppercase 
x <-gsub("[\u00c4\u00c0\u00c1\u00c2\u00c3\u00c5\u00c6]","A",x)
# A<-("\u00c4\u00c0\u00c1\u00c2\u00c3\u00c5\u00c6")
x <-gsub("[\u00cb\u00c8\u00c9\u00ca]","E",x)
# E<-("\u00cb\u00c8\u00c9\u00ca")
x <-gsub("[\u00cf\u00cc\u00cd\u00ce]","I",x)
x <-gsub("[\u00d6\u00d2\u00d3\u00d4\u00d5]","O",x)
# O<-("\u00d6\u00d2\u00d3\u00d4\u00d5")
x <-gsub("[\u00dc\u00d9\u00da\u00db]","U",x)
x <-gsub("\u00c7","C", x)
x <-gsub("\u00d1","N",x)
x <-gsub("[\u00dd\u009f]","Y",x)

# Will solve these marks: ß, à, á, â, ã, ä, å, æ, ç, è, 
# é, ê, ë, ì, í, î, ï, ñ, ò, ó, ô, õ, ö, ù, ú, û, ü, ý, ÿ, œ
# lowercase
x <-gsub("[\u00e4\u00e0\u00e1\u00e2\u00e3\u00e5\u00e6]","a",x)
# a<-("\u00e4\u00e0\u00e1\u00e2\u00e3\u00e5\u00e6")
x <-gsub("[\u00eb\u00e8\u00e9\u00ea]","e",x)
# e<-("\u00eb\u00e8\u00e9\u00ea")
x <-gsub("[\u00ef\u00ec\u00ed\u00ee]","i",x)
# i <-("\u00ef\u00ec\u00ed\u00ee")
x <-gsub("[\u00f6\u00f2\u00f3\u00f4\u00f5\u009c]","o",x)
x <-gsub("[\u00fc\u00f9\u00fa\u00fb]","u",x)
x <-gsub("\u00e7","c",x)
x <-gsub("\u00f1","n",x)
x <-gsub("[\u00fd\u00ff]","y",x)

return(x)
}
