# Contibuting to Shiny_DataScience

## Welcome contributors to the project: 

## Short Links to Important Resources:
* <a href="https://github.com/mscsalex/Shiny_DataScience/issues">bugs</a>: issue tracker / bug report tool
## Roadmap:
<ul>
 <li>Make the interface more informative</li>
 <li>Allow users to fork from this tool itself</li>
 <li>Add a forum to allow beginners to find potential mentors and help</li>
 </ul>

## Development environment details
You will need R installed on your system. 
Check <a href="https://github.com/mscsalex/Shiny_DataScience/blob/master/README.md">README</a> for more details.

## How to submit changes: Pull Request protocol etc. 
To submit a contribution:
<ol>
 <li><a href="github.com/mscsalex/Shiny_DataScience/fork">Fork</a> the repository.</li>
 <li>Clone the original repository to your local PC</a></li>
 <li>Add your fork with <code>git@github.com:username/Shiny_DataScience.git</code></li>
 <li>If you already have a fork and copy of the repo, you can simply check to make sure you're up-to-date
 <ul>Pull the upstream:<code>git pull upstream --rebase</code></ul>
 <ul>Update the submodules:<code>git submodule update --recursive --init</code></ul>
 </li>
 <li>Create a separate branch (recommended if you're making multiple changes simultaneously) 
 <ul>(<code>git checkout -b my-branch</code>)</ul></li>
 <li>Make changes</li>
 <li>. Commit (<code>git add <item(s) you changed>; git commit</code>) and write your commit message</li>
<li>Pull from upstream (<code>git pull upstream --rebase</code>) (Note: Make sure to include <code>--rebase</code>, as it will apply your changes on top of the changes you just pulled, allowing for a much cleaner merge)</li>
<li>Push to your fork (<code>git push origin _branch_</code>), <code>_branch_</code> being the branch you created earlier</li>
 <li>Create a pull request</li>
 <li>Describe changes</li>
 <li>Submit!</li>
 
 </ol>
 
## How to report a bug: 
* Templates: 
  *If you find a bug, you can also file an issue. Please provide as much relevant information as you can, and include a minimal reproducible example if possible. Add label "bug-report"
* First bugs for Contributors
  *Look for issues tagged with the label "good first issue" or "beginner". Feel free to play with the app and find any bugs(just do not compromise the server).
    
## New Feature Requirements
 * Open requests for new features as an issue, and add the label "enhancement-request"

## Style Guide / Coding conventions 
Remember to do a <code>git rebase -i upstream/master</code> before you send your patch!
Make sure your code is readable, commented, and well-documented.
Follo they <a href="https://google.github.io/styleguide/Rguide.xml">Google R style guide</a>

## Code of Conduct
Please be courtous. While we tru to merge pull requests after verificatio, this may take some time. Be patient.
Do no post vulgar/obscene comments either in issues, in commits or anywhere in this project, including the code.

## Recognition model
Contributers will be added to the <a href="https://github.com/mscsalex/Shiny_DataScience/blob/master/CONTRIBUTORS.md">Contributers</a> file

## Where can I ask for help?
Don't hesitate to ask for help, comments or suggestions!
Before implementing something, discuss it! Open an issue.
