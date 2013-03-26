<actionDetails>
  
  <div class="row-fluid">
    <div class="span6">

      <h4>Timestamp</h4>
      <p><loggedActionAt/></p>

      <h4>User</h4>
      <p><loggedActionUserName/></p>

      <h4>Entity Name</h4>
      <p><loggedActionEntityName/></p>

      <h4>Entity ID</h4>
      <p><loggedActionEntityId/></p>

      <h4>Action Type</h4>
      <p><loggedActionAction/></p>
    </div>

  </div>

  <div class="form-actions">
    <a href="${actionlogIndexPath}" class="btn">Return To Listing</a>
  </div>

  <table class="table table-striped">
    <tr>
      <th>Field</th>
      <th>Old Value</th>
      <th>New Value</th>
    </tr>
    <loggedActionDetails>
      <tr>
        <td><loggedActionDetailsFieldName/></td>
        <td><loggedActionDetailsOldValue/></td>
        <td><loggedActionDetailsNewValue/></td>
      </tr>
    </loggedActionDetails>
  </table>

</actionDetails>
