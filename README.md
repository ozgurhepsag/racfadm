# racfadm
Simplified RACF Administration ISPF Dialog

*---------------------------------------------------------------------*
*                            Installation                             *
*---------------------------------------------------------------------*
There are three installation options depending upon how the repository
was cloned.

Clone via ZIGI
--------------
The z/OS datasets will have been created by ZIGI during
the clone process. The installation at this point proceeds with the next
step - making the dialog available for use.

Clone using Git on a workstation
--------------------------------
The repositories files need to be uploaded to an OMVS directory. Then
proceed as if the Clone occurred under OMVS using Git.

Clone using Git under OMVS
--------------------------
This option requires that the Git client be installed under OMVS.
Once the clone completes, enter OMVS and change to the repositories
OMVS directory. Execute the zinstall script:

   /u/racfadm>./zginstall.rex

When the zinstall script ends there will be instructions on how to
apply the ISPF statistics to the partitioned dataset members.

Making the Dialog Available
---------------------------
To invoke this application execute the RACFADM exec:

           EX 'hlq.RACFADM.EXEC(RACFADM)' EX

The intended users must have access to these datasets.

*---------------------------------------------------------------------*
*                            Introduction                             *
*---------------------------------------------------------------------*
RACF Administration (RACFADM) makes many security tasks simple.  It lists
user, group, data set, and general resource profiles by means of a
user-friendly, menu-driven interface; it provides interactive modification
of most fields.

Among its features are: connecting groups to a user, adding permissions,
user authorization searching across classes, and displaying the group from
which an authorization is granted.

Written in customizable Rexx, RACFADM includes an exec to automate creation
of aliases or data sets when creating a new TSO user and can browse all RACF
system options from a single, scrollable display.

To begin using execute the RACFADM member found in the EXEC library.

# Contributors

* Nico Rizzuto (Creator)      [http://www.rizzuto.it](http://www.rizzuto.it)
* Lionel B. Dyck              [https://www.lbdsoftware.com/](https://www.lbdsoftware.com)
* Bruce Koss
* John Kalinich
* Bill Smith
