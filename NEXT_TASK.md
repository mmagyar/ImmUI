create text renderer
in order to show text
because it's not very useful without it

solve overlap issue
in order to properly display elements at all resultions
because the last elements ends at `10`, the next must begin in `10`, because if the last element would end at `9`, there would be a gap at 9.5 at higher resolution

#Do it later, not urgent

###Serializable
Handle serialization. Behaviour makes this hard. Options:

 - Handle behaviour in parallel. This might be hard? Also behaviour is part of the object