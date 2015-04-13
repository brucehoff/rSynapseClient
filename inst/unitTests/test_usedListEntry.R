# Unit tests for UsedListEntiry
# 
# Author: brucehoff
###############################################################################


unitTestUsedEntityListEntry<-function() {
	usedEntry<-synapseClient:::usedListEntry(listEntry="syn123456", wasExecuted=T)
	checkTrue(is(usedEntry, "UsedEntity"))
	checkEquals("syn123456", usedEntry$reference$targetId)
}

unitTestUsedURLListEntry<-function() {
	usedEntry<-synapseClient:::usedListEntry(listEntry="http://foo.bar", wasExecuted=F)
	checkTrue(!usedEntry$wasExecuted)
	checkTrue(is(usedEntry, "UsedURL"))
	checkEquals("http://foo.bar", usedEntry$url)
	checkEquals("http://foo.bar", usedEntry$name)
}

unitTestCharacterEntry<-function() {
	usedEntry<-synapseClient:::usedListEntry("http://foo.bar")
	checkTrue(!usedEntry$wasExecuted)
	checkEquals("http://foo.bar", usedEntry$url)
	checkEquals("http://foo.bar", usedEntry$name)
}
