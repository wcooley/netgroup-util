{-
 - Wil Cooley <wcooley@pdx.edu>
 - CS 457
 - Winter 2011
 -
 - netgroup-graph - Generate GraphViz dotfile from LDAP netgroups.
 -}

import LDAP.Netgroup
import Netgroup

main = do   ngs <- build_netgroups_from_ldap
            putStr $ netgroupGraph ngs
