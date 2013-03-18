<table class="table table-striped actionList">
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
        <td><a href="${actionlogItemShowUrl}"><loggedActionAt/></a></td>
        <td><loggedActionUserName/></td>
        <td><loggedActionEntityName/></td>
        <td><loggedActionEntityId/></td>
        <td><loggedActionAction/></td>
      </tr>
    </actionLogListing>
  </tbody>
</table>

<script >
  $(function() {
  $("table.actionList").tablesorter({ sortList: [[0,0]] });
  });
</script>

