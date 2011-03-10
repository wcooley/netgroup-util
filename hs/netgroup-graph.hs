{-
 - Wil Cooley <wcooley@pdx.edu>
 - CS 457
 - Winter 2011
 -
 - netgroup-graph - Generate GraphViz dotfile from LDAP netgroups.
 -}

import LDAP.Netgroup

main = do genGraph
