Types as Unit Tests
-------------------

There are lots of claims made about how types can replace whole categories of testing.  Usually,
the one most targeted is unit testing.  Many programmers steeped in TDD are rightfully suspicious
of such a claim. This is especially true of programmers who work in untyped languages like javascript or ruby, but who came from a language like C# or Java.  

I want to explain what I think most haskell or other typed language programmers mean when they make such a claim (though I am going to use strictly haskell).  

I won't say:
**"Type systems mean the end of unit tests!"**

Instead I'll say:
**"Careful selection of types can guarantee properties often tested for in unit test enviornments"**

I hope to create a somewhat useful library while doing it.

## Building something (somewhat) useful

A problem that comes up often at my work is generating a report from data.  The issue is, there is some very dynamic data source, say an xml file or a JSON object.  It has some sort of tree structure like:

``` javascript
{table-description:{company-name:"Talula Tablets"
                   ,purpose: "More and more tablets"
                   }
}

{table-data: {users:
          [ {user:{
                   user-name: "jack"
                  ,user-email: "jack@upthehill.com"
                  ,address: "Madil Oklahoma"
                  ,widgets: 47
                  {cog-data:{cogs:    16
                            ,cogId:   "#77s77"
                            ,favorite-toy: 
                            }
                  }
            }
           ,{user:{user-name:  "jill"
                  ,user-email: "jill@upthehill.com"
                  ,address:    "Okmulgee Oklahoma"
                  ,widgets:    66
                  {cog-data: { cogs:       3
                            , cogId:     "#66Ssss"
                            }
                  }
            }
           ,{user:{
                   user-name:  "muffet"
                  ,user-email: "muffet@on.atuffet.com"
                  ,address:    "Marlow Oklahoma"
                  ,widgets:    22
                  {cog-data:{cogs:       1
                           ,cogId:      "#55Ssss"
                           ,catch-phrase: "It's your world"}
                  }
            }
          ]
       }
}

```

## The problem is, the user wants the data formatted like this:

<hr>
<h1> Talula Tablets </h1>
<h2> Rendered this fine Monday Morning </h2>
<table class="pure-table">
 <thead>
 <tr>
  <th> User Name </th><th>Widget Count </th><th> Cog Id </th><th> Cog Count </th> <th> Widget Factor (Cogs + Widgets) </th>
 </tr>
 </thead>
 <tbody>
         <tr>
           <td>muffet</td><td>22</td><td>#55Ssss</td><td>1</td> <td>22</td>
         </tr>
         <tr>
           <td>jill</td><td>66</td><td>#66Ssss</td><td>3</td> <td>192</td>
         </tr>

 </tbody>
</table>

<hr>

There are several complicating factors in generating this report.

* The data is nested at several levels: ``` tables -> users -> cogs ```
* The data combines fields together: ``` cogs * widgets ```
* The table only uses a few of the available fields
* The table uses different labels than the json to name the data: *User Name* instead of *user-name*
* The table re-organizes the order of each field: *CogId* before *Cog*
* The table uses a completely different datatype to get the preamble info: *Tabula Tablets*
* The table also depends on a context not given to create the date: *A fine Monday Morning*
