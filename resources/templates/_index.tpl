<apply template='_application'>

  <bind tag='pageHeader'>
    Action Log
  </bind>

  <bind tag='pageTagline'>
    history of modifications to your data
  </bind>


  <bind tag='contentSecondary'>
    <h3>Action Log Filter</h3>
    <actionLogFilterForm method="GET">
      <dfErrorList ref=""/>

      <dfLabel ref="user">User</dfLabel>
      <dfInputSelect ref="user" />
      <dfErrorList ref="user"/>
      <br>

      <dfLabel ref="entity">Entity</dfLabel>
      <dfInputSelect ref="entity" disableonsingle/>
      <dfErrorList ref="entity"/>
      <br>

      <dfLabel ref="entity-id">Entity ID</dfLabel>
      <dfInputText ref="entity-id" disableonsingle/>
      <dfErrorList ref="entity-id"/>
      <br>

      <dfLabel ref="action">Action</dfLabel>
      <dfInputSelect ref="action" />
      <dfErrorList ref="action"/>
      <br>

      <dfInputSubmit class="btn btn-success" value="Refresh"/>
    </actionLogFilterForm>
  </bind>

  <bind tag='contentMain'>
    
    <table class="table table-striped userList">
      <thead>
        <tr>
          <th>Timestamp</th>
          <th>User</th>
          <th>Entity Name</th>
          <th>Entity ID</th>
          <th>Action</th>
        </tr>
      </thead>
      <tbody>
        <actionLogListing>
          <tr>
<!--            <td><a href="${actionlogItemShowPath}"><loggedActionAt/></a></td> -->
            <td><loggedActionAt/></td>
            <td><loggedActionUserId/></td>
            <td><loggedActionEntityName/></td>
            <td><loggedActionEntityId/></td>
            <td><loggedActionAction/></td>
          </tr>
        </actionLogListing>
      </tbody>
    </table>

    <script >
      $(function() {
      $("table.userList").tablesorter({ sortList: [[0,0]] });
      });
    </script>

  </bind>

</apply>


