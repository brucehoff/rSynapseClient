#
# Methods for creating a UsedEntity from an Entity or a URL
# The Entity may either be a SynapseEntity object, an entity ID, 
# or a Reference
#
setMethod(
  f="usedListEntry",
  signature = signature("Entity"),
  definition = function(listEntry, ...) {
    otherParams<-list(...)
    if (is.null(otherParams$wasExecuted)) {
      wasExecuted=F
    } else {
      wasExecuted<-otherParams$wasExecuted
    }
    UsedEntity(reference=getReference(listEntry), wasExecuted=wasExecuted)
  }
)

setMethod(
  f="usedListEntry",
  signature = signature("character"),
  definition = function(listEntry, ...) {
    otherParams<-list(...)
    if (is.null(otherParams$wasExecuted)) {
      wasExecuted=F
    } else {
      wasExecuted<-otherParams$wasExecuted
    }
    if (isSynapseId(listEntry)) {
			UsedEntity(reference=getReference(listEntry), wasExecuted=wasExecuted)
    } else {
      # must be a URL
			UsedURL(url=listEntry, name=listEntry, wasExecuted=wasExecuted)
    }
  }
)

setMethod(
	f="usedListEntry",
	signature = signature("UsedEntity"),
	definition = function(listEntry, ...) {
		if (is.null(listEntry$wasExecuted)) {
			otherParams<-list(...)
			if (is.null(otherParams$wasExecuted)) {
				listEntry$wasExecuted=F
			} else {
				listEntry$wasExecuted<-otherParams$wasExecuted
			}
		}
		listEntry
	}
)

setMethod(
	f="usedListEntry",
	signature = signature("UsedURL"),
	definition = function(listEntry, ...) {
		if (is.null(listEntry$wasExecuted)) stop("'wasExecuted' required.")
		if (is.null(listEntry$name)) listEntry$name<-listEntry$url
		listEntry
	}
)


setMethod(
	f="usedListEntry",
	signature = signature("Reference"),
	definition = function(listEntry, ...) {
		otherParams<-list(...)
		if (is.null(otherParams$wasExecuted)) {
			wasExecuted=F
		} else {
			wasExecuted<-otherParams$wasExecuted
		}
		UsedEntity(reference=listEntry, wasExecuted=wasExecuted)
	}
)

