#!/usr/bin/env python26
#
# Name: ldap-to-netgroup.py
# Desc: Generate /etc/netgroup from LDAP
#
# Written by Wil Cooley <wcooley (at) nakedape.cc>
#

import ldap
from pprint import pprint, pformat
import sys

ldap_conn = {
        uri = 'ldaps://ldap.example.com',
        basedn = 'dc=example,dc=com',
}

class Netgroup(object):

    @staticmethod
    def from_ldap(ldap_entry):
        ng = Netgroup()
        ng.name = ldap_entry['cn'][0]

        ng.members = []
        if ldap_entry.has_key('nisNetgroupTriple'):
            ng.members.extend(ldap_entry['nisNetgroupTriple'])

        if ldap_entry.has_key('memberNisNetgroup'):
            ng.members.extend(ldap_entry['memberNisNetgroup'])

        ng.description = None
        if ldap_entry.has_key('description'):
            ng.description = ldap_entry['description'][0]

        return ng

    def __repr__(self):
        return pformat({'name': self.name, 'members': self.members})

    def __str__(self):
        s = ''
        if self.description is not None:
            s += "# " + self.description + "\n"

        s += "%-21s %s" % (self.name, ' '.join(self.members))
        return s


if __name__ == '__main__':
    l = ldap.initialize(ldap_conn['uri'])

    l.simple_bind_s()

    result = l.search_s(ldap_conn['basedn'], ldap.SCOPE_SUBTREE, 'objectclass=nisnetgroup',
                ['cn', 'description', 'memberNisNetgroup', 'nisNetgroupTriple'])

    userngs = []
    hostngs = []

    for netgroup in sorted(result):
        netgroup = Netgroup.from_ldap(netgroup[1])

        if netgroup.name.endswith('-access'):
            userngs.append(netgroup)
        else:
            hostngs.append(netgroup)

    print "# Generated file - DO NOT EDIT"
    print "#\n# User-access netgroups\n#"
    for ng in userngs: print ng, "\n"
    print "#\n# Host netgroups\n#"
    for ng in hostngs: print ng, "\n"
    print
